.. Copyright (C) 2005 - 2009  Eric Van Dewoestine

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.

.. _vim/c/hierarchy:

.. _\:CCallHierarchy:

Call Hierarchy
==============

When viewing a c or c++ source file you can view the call hierarchy of a
function or method by issuing the command **:CCallHierarchy**.  This will open
a temporary buffer with an inversed tree view of the hierarchy of callers of
the requested function or method.

.. code-block:: c

  fun2(int)
     fun1(int)
       main()
       fun3(int)
     fun3(int)

While you are in the hierarchy tree buffer, you can jump to the call under the
cursor using one of the following key bindings:

  - <cr> - open the type using the
    (:ref:`default action <g:EclimCHierarchyDefaultAction>`).
  - E - open the type via :edit
  - S - open the type via :split
  - T - open the type via :tabnew
  - ? - view help buffer


Configuration
-------------

Vim Variables

.. _g\:EclimCHierarchyDefaultAction:

- **g:EclimCHierarchyDefaultAction** (defaults to 'split') -
  Determines the command used to open the file when hitting <enter> on an entry
  in the hierarchy buffer.
