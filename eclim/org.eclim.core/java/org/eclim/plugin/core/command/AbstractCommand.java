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
package org.eclim.plugin.core.command;

import java.util.Arrays;

import org.eclim.command.Command;
import org.eclim.command.CommandLine;
import org.eclim.command.Options;

import org.eclim.logging.Logger;

import org.eclim.plugin.core.preference.Preferences;

import org.eclim.plugin.core.util.ProjectUtils;

import org.eclim.util.file.FileUtils;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;

import com.martiansoftware.nailgun.NGContext;

/**
 * Abstract implmentation of {@link Command}.
 *
 * @author Eric Van Dewoestine
 */
public abstract class AbstractCommand
  implements Command
{
  private static final Logger logger = Logger.getLogger(AbstractCommand.class);

  private NGContext context;

  /**
   * Gets the preferences.
   *
   * @return Preferences.
   */
  public Preferences getPreferences()
  {
    return Preferences.getInstance();
  }

  /**
   * Convenience method which uses the standard project, file, offset, and
   * encoding options to determine the character offset in the file.
   *
   * @param commandLine The command line instance.
   * @return The char offset.
   */
  public int getOffset(CommandLine commandLine)
    throws Exception
  {
    String project = commandLine.getValue(Options.PROJECT_OPTION);
    if (project == null){
      // some commands use -n for the project name (like all the search commands)
      project = commandLine.getValue(Options.NAME_OPTION);
    }
    String file = commandLine.getValue(Options.FILE_OPTION);
    String encoding = commandLine.getValue(Options.ENCODING_OPTION);
    int offset = Integer.parseInt(
        commandLine.getValue(Options.OFFSET_OPTION));
    file = ProjectUtils.getFilePath(project, file);

    return FileUtils.byteOffsetToCharOffset(file, offset, encoding);
  }

  /**
   * {@inheritDoc}
   * @see Command#getContext()
   */
  public NGContext getContext()
  {
    return context;
  }

  /**
   * {@inheritDoc}
   * @see Command#setContext(NGContext)
   */
  public void setContext(NGContext context)
  {
    this.context = context;
  }

  /**
   * {@inheritDoc}
   * @see Command#cleanup(CommandLine)
   */
  public void cleanup(CommandLine commandLine)
  {
    try{
      // cleanup temp files
      if (commandLine.hasOption(Options.FILE_OPTION) &&
          (commandLine.hasOption(Options.PROJECT_OPTION) ||
           commandLine.hasOption(Options.NAME_OPTION)))
      {
        IProject project = null;
        String projectName = commandLine.getValue(Options.PROJECT_OPTION);
        if (projectName == null){
          // some commands use -n for the project name (like all the search commands)
          projectName = commandLine.getValue(Options.NAME_OPTION);
        }else{
          // some commands use -n for the project name but also have a -p option
          // (like the search commands when searching by pattern)
          project = ProjectUtils.getProject(projectName);
          if (!project.exists()){
            project = null;
            projectName = commandLine.getValue(Options.NAME_OPTION);
          }
        }

        if (project == null && projectName != null){
          project = ProjectUtils.getProject(projectName);
        }

        String file = commandLine.getValue(Options.FILE_OPTION);
        if (project != null && project.exists() && file != null){
          IFile temp = ProjectUtils.getFile(project, file);
          if (temp.exists() && temp.getName().startsWith("__eclim_temp_")){
            temp.delete(true, false, null);
          }
        }
      }
    }catch(Exception e){
      logger.error("Exception during cleanup of command: " +
          Arrays.toString(context.getArgs()), e);
    }
  }

  public void println()
  {
    context.out.println();
  }

  public void println(boolean x)
  {
    context.out.println(x);
  }

  public void println(char x)
  {
    context.out.println(x);
  }

  public void println(char[] x)
  {
    context.out.println(x);
  }

  public void println(double x)
  {
    context.out.println(x);
  }

  public void println(float x)
  {
    context.out.println(x);
  }

  public void println(int x)
  {
    context.out.println(x);
  }

  public void println(long x)
  {
    context.out.println(x);
  }

  public void println(Object x)
  {
    context.out.println(x);
  }

  public void println(String x)
  {
    context.out.println(x);
  }
}
