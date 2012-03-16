/**
 * Copyright (C) 2005 - 2010  Eric Van Dewoestine
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
package org.eclim.plugin.dltkruby.command.src;

import org.eclim.annotation.Command;

import org.eclim.plugin.dltk.command.src.AbstractSrcUpdateCommand;

import org.eclipse.dltk.ruby.core.RubyNature;

/**
 * Command to update and optionally validate a ruby src file.
 *
 * @author Eric Van Dewoestine
 */
@Command(
  name = "ruby_src_update",
  options =
    "REQUIRED p project ARG," +
    "REQUIRED f file ARG," +
    "OPTIONAL v validate NOARG," +
    "OPTIONAL b build NOARG"
)
public class SrcUpdateCommand
  extends AbstractSrcUpdateCommand
{
  /**
   * {@inheritDoc}
   * @see AbstractSrcUpdateCommand#getNature()
   */
  @Override
  protected String getNature()
  {
    return RubyNature.NATURE_ID;
  }
}
