/**
 * Copyright (C) 2005 - 2011  Eric Van Dewoestine
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
package org.eclim.plugin.jdt.command.include;

import java.lang.reflect.Method;

import java.util.ArrayList;

import org.eclim.annotation.Command;

import org.eclim.command.CommandLine;
import org.eclim.command.Options;

import org.eclim.plugin.core.command.AbstractCommand;

import org.eclim.plugin.core.util.ProjectUtils;

import org.eclipse.core.resources.IProject;

import org.eclipse.core.runtime.IStatus;

import org.eclipse.jdt.internal.ui.preferences.ImportOrganizeConfigurationBlock;
import org.eclipse.jdt.internal.ui.preferences.ImportOrganizeConfigurationBlock.ImportOrderEntry;

import org.eclipse.jdt.internal.ui.wizards.IStatusChangeListener;

/**
 * Command to retrieve the eclipse configured import order.
 *
 * @author Eric Van Dewoestine
 */
@Command(
  name = "java_import_order",
  options = "REQUIRED p project ARG"
)
public class ImportOrderCommand
  extends AbstractCommand
{
  /**
   * {@inheritDoc}
   * @see org.eclim.command.Command#execute(CommandLine)
   */
  public Object execute(CommandLine commandLine)
    throws Exception
  {
    String projectName = commandLine.getValue(Options.PROJECT_OPTION);
    final IProject project = ProjectUtils.getProject(projectName);

    ImportOrganizeConfigurationBlock block =
      new ImportOrganizeConfigurationBlock(new IStatusChangeListener(){
        public void statusChanged(IStatus status){
          // no-op
        }
      }, project, null);

    Method getImportOrderPreference = ImportOrganizeConfigurationBlock.class
      .getDeclaredMethod("getImportOrderPreference");
    getImportOrderPreference.setAccessible(true);
    ImportOrderEntry[] entries = (ImportOrderEntry[])
      getImportOrderPreference.invoke(block);

    ArrayList<String> results = new ArrayList<String>();
    for (ImportOrderEntry entry : entries){
      results.add(entry.name);
    }
    return results;
  }
}
