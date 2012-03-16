/**
 * Copyright (C) 2005 - 2012  Eric Van Dewoestine
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.eclim.plugin.jdt.command.src;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

import java.util.ArrayList;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.BuildLogger;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;

import org.apache.tools.ant.taskdefs.ExecuteStreamHandler;
import org.apache.tools.ant.taskdefs.Java;
import org.apache.tools.ant.taskdefs.PumpStreamHandler;
import org.apache.tools.ant.taskdefs.Redirector;

import org.apache.tools.ant.types.Commandline.Argument;

import org.apache.tools.ant.types.Environment.Variable;
import org.apache.tools.ant.types.Path;

import org.eclim.Services;

import org.eclim.annotation.Command;

import org.eclim.command.CommandLine;
import org.eclim.command.Options;

import org.eclim.plugin.core.command.AbstractCommand;

import org.eclim.plugin.core.util.ProjectUtils;

import org.eclim.plugin.jdt.util.ClasspathUtils;
import org.eclim.plugin.jdt.util.JavaUtils;

import org.eclim.util.StringUtils;

import org.eclipse.core.resources.IProject;

import org.eclipse.core.runtime.IPath;

import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IPackageDeclaration;

import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jdt.core.search.IJavaSearchScope;
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.jdt.core.search.SearchMatch;
import org.eclipse.jdt.core.search.SearchParticipant;
import org.eclipse.jdt.core.search.SearchPattern;
import org.eclipse.jdt.core.search.SearchRequestor;

import com.google.gson.Gson;

/**
 * Command to run the project's main class.
 *
 * @author Eric Van Dewoestine
 */
@Command(
  name = "java",
  options =
    "REQUIRED p project ARG," +
    "OPTIONAL d debug NOARG," +
    "OPTIONAL c classname ARG," +
    "OPTIONAL w workingdir ARG," +
    "OPTIONAL v vmargs ANY," +
    "OPTIONAL s sysprops ANY," +
    "OPTIONAL e envargs ANY," +
    "OPTIONAL a args ANY"
)
public class JavaCommand
  extends AbstractCommand
{
  private static final String WORKINGDIR_OPTION = "w";
  private static final String VMARGS_OPTION = "v";
  private static final String SYSPROPS_OPTION = "s";
  private static final String ENVARGS_OPTION = "e";

  /**
   * {@inheritDoc}
   * @see org.eclim.command.Command#execute(CommandLine)
   */
  public Object execute(CommandLine commandLine)
    throws Exception
  {
    String projectName = commandLine.getValue(Options.PROJECT_OPTION);
    String mainClass = commandLine.getValue(Options.CLASSNAME_OPTION);
    boolean debug = commandLine.hasOption(Options.DEBUG_OPTION);
    String workingDir = commandLine.getValue(WORKINGDIR_OPTION);

    IProject project = ProjectUtils.getProject(projectName);
    IJavaProject javaProject = JavaUtils.getJavaProject(project);

    Project antProject = new Project();
    BuildLogger buildLogger = new DefaultLogger();
    buildLogger.setMessageOutputLevel(debug ? Project.MSG_DEBUG : Project.MSG_INFO);
    buildLogger.setOutputPrintStream(getContext().out);
    buildLogger.setErrorPrintStream(getContext().err);
    antProject.addBuildListener(buildLogger);
    antProject.setBasedir(ProjectUtils.getPath(project));
    antProject.setDefaultInputStream(System.in);

    if (mainClass == null){
      mainClass =
        ProjectUtils.getSetting(project, "org.eclim.java.run.mainclass");
    }

    if (mainClass == null ||
        mainClass.trim().equals(StringUtils.EMPTY) ||
        mainClass.trim().equals("none"))
    {
      // first try to locate a main method.
      mainClass = findMainClass(javaProject);
      if (mainClass == null){
        throw new RuntimeException(Services.getMessage(
              "setting.not.set", "org.eclim.java.run.mainclass"));
      }
    }

    if (mainClass.endsWith(".java") || mainClass.endsWith(".class")){
      mainClass = mainClass.substring(0, mainClass.lastIndexOf('.'));
    }

    Java java = new MyJava();
    java.setTaskName("java");
    java.setProject(antProject);
    java.setClassname(mainClass);
    java.setFork(true);

    if (workingDir != null){
      java.setDir(new File(workingDir));
    }

    // construct classpath
    Path classpath = new Path(antProject);
    String[] paths = ClasspathUtils.getClasspath(javaProject);
    for (String path : paths){
      Path.PathElement pe = classpath.createPathElement();
      pe.setPath(path);
    }

    java.setClasspath(classpath);

    // add default vm args
    String setting = ProjectUtils.getSetting(project, "org.eclim.java.run.jvmargs");
    if (setting != null && !setting.trim().equals(StringUtils.EMPTY)){
      String[] defaultArgs = (String[])new Gson().fromJson(setting, String[].class);
      if (defaultArgs != null && defaultArgs.length > 0){
        for(String vmarg : defaultArgs){
          if (!vmarg.startsWith("-")){
            continue;
          }
          Argument a = java.createJvmarg();
          a.setValue(vmarg);
        }
      }
    }

    // add any supplied vm args
    String[] vmargs = commandLine.getValues(VMARGS_OPTION);
    if (vmargs != null && vmargs.length > 0){
      for(String vmarg : vmargs){
        if (!vmarg.startsWith("-")){
          continue;
        }
        Argument a = java.createJvmarg();
        a.setValue(vmarg);
      }
    }

    // add any supplied system properties
    String[] props = commandLine.getValues(SYSPROPS_OPTION);
    if (props != null && props.length > 0){
      for(String prop : props){
        String[] sysprop = StringUtils.split(prop, "=", 2);
        if (sysprop.length != 2){
          continue;
        }
        if (sysprop[0].startsWith("-D")){
          sysprop[0] = sysprop[0].substring(2);
        }
        Variable var = new Variable();
        var.setKey(sysprop[0]);
        var.setValue(sysprop[1]);
        java.addSysproperty(var);
      }
    }

    // add any env vars
    String[] envs = commandLine.getValues(ENVARGS_OPTION);
    if (envs != null && envs.length > 0){
      for(String env : envs){
        String[] envvar = StringUtils.split(env, "=", 2);
        if (envvar.length != 2){
          continue;
        }
        Variable var = new Variable();
        var.setKey(envvar[0]);
        var.setValue(envvar[1]);
        java.addEnv(var);
      }
    }

    // add any supplied command line args
    String[] args = commandLine.getValues(Options.ARGS_OPTION);
    if (args != null && args.length > 0){
      for(String arg : args){
        Argument a = java.createArg();
        a.setValue(arg);
      }
    }

    java.execute();

    return null;
  }

  private String findMainClass(IJavaProject javaProject)
    throws Exception
  {
    final String projectPath = ProjectUtils.getPath(javaProject.getProject());
    final ArrayList<IMethod> methods = new ArrayList<IMethod>();
    int context = IJavaSearchConstants.DECLARATIONS;
    int type = IJavaSearchConstants.METHOD;
    int matchType = SearchPattern.R_EXACT_MATCH | SearchPattern.R_CASE_SENSITIVE;
    IJavaSearchScope scope =
      SearchEngine.createJavaSearchScope(new IJavaElement[]{javaProject});
    SearchPattern pattern =
      SearchPattern.createPattern("main(String[])", type, context, matchType);
    SearchRequestor requestor = new SearchRequestor(){
      public void acceptSearchMatch(SearchMatch match){
        if(match.getAccuracy() != SearchMatch.A_ACCURATE){
          return;
        }

        IPath location = match.getResource().getLocation();
        if (location == null){
          return;
        }

        String path = location.toOSString().replace('\\', '/');
        if (!path.toLowerCase().startsWith(projectPath.toLowerCase())){
          return;
        }

        IJavaElement element = (IJavaElement)match.getElement();
        if (element.getElementType() != IJavaElement.METHOD){
          return;
        }

        IMethod method = (IMethod)element;
        String[] params = method.getParameterTypes();
        if (params.length != 1){
          return;
        }
        methods.add(method);
      }
    };

    if(pattern != null){
      SearchEngine engine = new SearchEngine();
      SearchParticipant[] participants =
        new SearchParticipant[]{SearchEngine.getDefaultSearchParticipant()};
      engine.search(pattern, participants, scope, requestor, null);

      // if we found only 1 result, we can use it.
      if (methods.size() == 1){
        IMethod method = methods.get(0);
        ICompilationUnit cu = method.getCompilationUnit();
        IPackageDeclaration[] packages = cu.getPackageDeclarations();
        if (packages != null && packages.length > 0){
          return packages[0].getElementName() + "." + cu.getElementName();
        }
        return cu.getElementName();
      }
    }
    return null;
  }

  /* All of this is to ensure that System.out calls by the running class are
   * flushed immediatly to the console for things like user input prompts. */

  private class MyJava
    extends Java
  {
    public MyJava()
    {
      super();
      this.redirector = new MyRedirector(this);
    }
  }

  private class MyRedirector
    extends Redirector
  {
    public MyRedirector(Task task)
    {
      super(task);
    }

    @Override
    public synchronized ExecuteStreamHandler createHandler()
      throws BuildException
    {
      return new MyPumpStreamHandler();
    }

    @Override
    public synchronized void complete() throws IOException
    {
      getContext().out.flush();
      getContext().err.flush();
    }
  }

  private class MyPumpStreamHandler
    extends PumpStreamHandler
  {
    public MyPumpStreamHandler()
    {
      super(new FlushingOutputStream(
            getContext().out), getContext().err, getContext().in);
    }
  }

  private class FlushingOutputStream
    extends OutputStream
  {
    private OutputStream out;

    public FlushingOutputStream(OutputStream out)
    {
      this.out = out;
    }

    @Override
    public void write(int b)
      throws IOException
    {
      out.write(b);
      out.flush();
    }

    @Override
    public void write(byte[] b)
      throws IOException
    {
      out.write(b);
      out.flush();
    }

    @Override
    public void write(byte[] b, int off, int len)
      throws IOException
    {
      out.write(b, off, len);
      out.flush();
    }
  }
}
