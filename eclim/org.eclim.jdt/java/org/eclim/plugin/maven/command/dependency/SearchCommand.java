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
package org.eclim.plugin.maven.command.dependency;

import java.io.FileInputStream;
import java.io.InputStream;

import java.net.URL;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.xml.parsers.DocumentBuilderFactory;

import org.eclim.annotation.Command;

import org.eclim.command.CommandLine;
import org.eclim.command.Options;

import org.eclim.logging.Logger;

import org.eclim.plugin.core.command.AbstractCommand;

import org.eclim.plugin.core.util.ProjectUtils;
import org.eclim.plugin.core.util.XmlUtils;

import org.eclim.util.IOUtils;

import org.w3c.dom.NodeList;

import net.htmlparser.jericho.Element;
import net.htmlparser.jericho.Source;

/**
 * Command for searching online maven repository.
 *
 * @author Eric Van Dewoestine
 */
@Command(
  name = "maven_dependency_search",
  options =
    "REQUIRED p project ARG," +
    "REQUIRED f file ARG," +
    "REQUIRED t type ARG," +
    "REQUIRED s search ARG"
)
public class SearchCommand
  extends AbstractCommand
{
  private static final Logger logger = Logger.getLogger(SearchCommand.class);

  // Note: Experienced connections issues not long after switching to jarvana.
  // If these continue, consider switching to another alternate or possibly
  // implement primary secondary support.
  //   http://www.mavensearch.net/search?q=junit
  private static final String URL =
    "http://www.jarvana.com/jarvana/search?search_type=project&project=";

  private static final String GROUP_ID = "Group Id";
  private static final String ARTIFACT_ID = "Artifact Id";
  private static final String VERSION = "Version";

  private static final String IVY = "ivy";
  private static final String DEPENDENCIES = "dependencies";
  private static final String DEPENDENCY = "dependency";

  /**
   * {@inheritDoc}
   */
  public Object execute(CommandLine commandLine)
    throws Exception
  {
    String search = commandLine.getValue(Options.SEARCH_OPTION);

    // existing dependencies
    List<Dependency> existing = null;
    try{
      String project = commandLine.getValue(Options.PROJECT_OPTION);
      String file = commandLine.getValue(Options.FILE_OPTION);
      String type = commandLine.getValue(Options.TYPE_OPTION);
      existing = getExistingDependencies(project, file, type);
    }catch(Exception e){
      logger.warn("Unable to get existing dependencies.", e);
      existing = new ArrayList<Dependency>();
    }

    return searchRepositories(search, existing);
  }

  /**
   * Searches the repositories supported by maven.ozacc.com.
   *
   * @param query The search query.
   * @return Possibly empty List of results.
   */
  private List<Dependency> searchRepositories(
      String query, List<Dependency> existing)
    throws Exception
  {
    ArrayList<Dependency> dependencies = new ArrayList<Dependency>();

    Source source = new Source(new URL(URL + query));
    Element table = source.getElementById("resulttable");
    if (table == null){
      return dependencies;
    }

    // get header column indexes
    int groupIndex = -1;
    int artifactIndex = -1;
    int versionIndex = -1;

    List<Element> ths = table.getAllElements("th");
    for (int ii = 0; ii < ths.size(); ii++){
      Element th = ths.get(ii);
      String text = th.getTextExtractor().toString().trim();
      if(groupIndex == -1 && GROUP_ID.equals(text)){
        groupIndex = ii;
      }else if(artifactIndex == -1 && ARTIFACT_ID.equals(text)){
        artifactIndex = ii;
      }else if(versionIndex == -1 && VERSION.equals(text)){
        versionIndex = ii;
      }

      if(groupIndex >= 0 && artifactIndex >= 0 && versionIndex >= 0){
        break;
      }
    }

    Iterator<Element> rows = table.getAllElements("tr").iterator();
    // skip header row
    rows.next();

    while (rows.hasNext()){
      Element row = rows.next();
      List<Element> cells = row.getAllElements("td");

      Dependency dependency = new Dependency();
      dependency.setGroupId(
          cells.get(groupIndex).getTextExtractor().toString().trim());
      dependency.setArtifactId(
          cells.get(artifactIndex).getTextExtractor().toString().trim());
      dependency.setVersion(
          cells.get(versionIndex).getTextExtractor().toString().trim());
      if (existing.contains(dependency)){
        dependency.setExisting(true);
      }
      dependencies.add(dependency);
    }

    return dependencies;
  }

  /**
   * Get the project file's current dependencies.
   *
   * @param project The eclipse project name.
   * @param filename The project file.
   * @param type The file type (ivy, maven, mvn).
   * @return List of dependencies.
   */
  private List<Dependency> getExistingDependencies(
      String project, String filename, String type)
    throws Exception
  {
    ArrayList<Dependency> list = new ArrayList<Dependency>();
    InputStream in = null;
    try{
      String file = ProjectUtils.getFilePath(project, filename);
      org.w3c.dom.Element root = DocumentBuilderFactory
        .newInstance()
        .newDocumentBuilder()
        .parse(in = new FileInputStream(file))
        .getDocumentElement();
      NodeList depends = root.getElementsByTagName(DEPENDENCIES);
      if (depends.getLength() > 0){
        NodeList nodes =
          ((org.w3c.dom.Element)depends.item(0)).getElementsByTagName(DEPENDENCY);

        for (int ii = 0; ii < nodes.getLength(); ii++){
          org.w3c.dom.Element element = (org.w3c.dom.Element)nodes.item(ii);

          Dependency dependency = new Dependency();
          if(IVY.equals(type)){
            dependency.setGroupId(element.getAttribute(Dependency.ORG));
            dependency.setArtifactId(element.getAttribute(Dependency.NAME));
            dependency.setVersion(element.getAttribute(Dependency.REV));
          }else{
            dependency.setGroupId(
                XmlUtils.getElementValue(element, Dependency.GROUP_ID));
            dependency.setArtifactId(
                XmlUtils.getElementValue(element, Dependency.ARTIFACT_ID));
            dependency.setVersion(
                XmlUtils.getElementValue(element, Dependency.VERSION));
          }

          list.add(dependency);
        }
      }
    }finally{
      IOUtils.closeQuietly(in);
    }
    return list;
  }
}
