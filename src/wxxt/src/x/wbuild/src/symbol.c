/*   wbuild
     Copyright (C) 1996  Joel N. Weber II <nemo@koa.iolani.honolulu.hi.us>
     
     This program is free software; you can redistribute it and/or
     modify it under the terms of the GNU General Public License
     as published by the Free Software Foundation; either version 2
     of the License, or (at your option) any later version.
     
     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.
     
     You should have received a copy of the GNU General Public License
     along with this program; if not, write to the Free Software
     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>

#include <stdio.h>

#include <libit/string.h>

#include <wbuild.h>
#include <wsym.h>

#include <libit/malloc.h>

/* These strings all pretty much translate into the text of the identifier.
 * Aparently they make comparisons and such easier.
 */
STRING DOLLAR;
STRING SELF;
STRING DOTDOTDOT;
STRING SYN_CONSTRAINT_RESOURCES;
STRING NUM_SYN_CONSTRAINT_RESOURCES;
STRING PARENT_PROCESS;
STRING MANAGER_EXTENSION;
STRING BORDER_HIGHLIGHT;
STRING BORDER_UNHIGHLIGHT;
STRING TRANSLATIONS;
STRING ARM_AND_ACTIVATE;
STRING SYN_RESOURCES;
STRING NUM_SYN_RESOURCES;
STRING PRIMITIVE_EXTENSION;
STRING CONSTRAINT_RESOURCES;
STRING CONSTRAINT_SIZE;
STRING CONSTRAINT_INITIALIZE;
STRING CONSTRAINT_DESTROY;
STRING CONSTRAINT_SET_VALUES;
STRING CONSTRAINT_EXTENSION;
STRING GEOMETRY_MANAGER;
STRING CHANGE_MANAGED;
STRING INSERT_CHILD;
STRING DELETE_CHILD;
STRING CLASS_INITIALIZE;
STRING CLASS_PART_INITIALIZE;
STRING INITIALIZE;
STRING INITIALIZE_HOOK;
STRING REALIZE;
STRING COMPRESS_MOTION;
STRING COMPRESS_EXPOSURE;
STRING COMPRESS_ENTERLEAVE;
STRING VISIBLE_INTEREST;
STRING DESTROY;
STRING RESIZE;
STRING EXPOSE;
STRING SET_VALUES;
STRING SET_VALUES_HOOK;
STRING SET_VALUES_ALMOST;
STRING GET_VALUES_HOOK;
STRING ACCEPT_FOCUS;
STRING QUERY_GEOMETRY;
STRING DISPLAY_ACCELERATOR;
STRING EXTENSION;
STRING CORE;
STRING RECTOBJ;
STRING COMPOSITE;
STRING SHELL;
STRING CONSTRAINT;
STRING OBJECT;
STRING OVERRIDESHELL;
STRING WMSHELL;
STRING VENDORSHELL;
STRING TRANSIENTSHELL;
STRING TOPLEVELSHELL;
STRING APPLICATIONSHELL;
STRING XMPRIMITIVE;
STRING PRIMITIVE;
STRING XMMANAGER;
STRING MANAGER;
struct _Section action_proto;

void symbol_init(void)
{
	SELF = hash("self");
	DOTDOTDOT = hash("...");
	SYN_CONSTRAINT_RESOURCES = hash("syn_constraint_resources");
	NUM_SYN_CONSTRAINT_RESOURCES = hash("num_syn_constraint_resources");
	PARENT_PROCESS = hash("parent_process");
	MANAGER_EXTENSION = hash("manager_extension");
	BORDER_HIGHLIGHT = hash("border_highlight");
	BORDER_UNHIGHLIGHT = hash("border_unhighlight");
	TRANSLATIONS = hash("translations");
	ARM_AND_ACTIVATE = hash("arm_and_activate");
	SYN_RESOURCES = hash("syn_resources");
	NUM_SYN_RESOURCES = hash("num_syn_resources");
	PRIMITIVE_EXTENSION = hash("primitive_extension");
	CONSTRAINT_RESOURCES = hash("constraint_resources");
	CONSTRAINT_SIZE = hash("constraint_size");
	CONSTRAINT_INITIALIZE = hash("constraint_initialize");
	CONSTRAINT_DESTROY = hash("constraint_destroy");
	CONSTRAINT_SET_VALUES = hash("constraint_set_values");
	CONSTRAINT_EXTENSION = hash("constraint_extension");
	GEOMETRY_MANAGER = hash("geometry_manager");
	CHANGE_MANAGED = hash("change_managed");
	INSERT_CHILD = hash("insert_child");
	DELETE_CHILD = hash("delete_child");
	DISPLAY_ACCELERATOR = hash("display_accelerator");  
	QUERY_GEOMETRY = hash("query_geometry");
	CLASS_INITIALIZE = hash("class_initialize");
	CLASS_PART_INITIALIZE = hash("class_part_initialize");
	INITIALIZE = hash("initialize");
	INITIALIZE_HOOK = hash("initialize_hook");
	REALIZE = hash("realize");
	COMPRESS_MOTION = hash("compress_motion");
	COMPRESS_EXPOSURE = hash("compress_exposure");
	COMPRESS_ENTERLEAVE = hash("compress_enterleave");
	VISIBLE_INTEREST = hash("visible_interest");
	DESTROY = hash("destroy");
	RESIZE = hash("resize");
	EXPOSE = hash("expose");
	SET_VALUES = hash("set_values");
	SET_VALUES_HOOK = hash("set_values_hook");
	SET_VALUES_ALMOST = hash("set_values_almost");
	GET_VALUES_HOOK = hash("get_values_hook");
	ACCEPT_FOCUS = hash("accept_focus");
	EXTENSION = hash("extension");
	CORE = hash("Core");
	RECTOBJ = hash("RectObj");
	OBJECT = hash("Object");
	COMPOSITE = hash("Composite");
	SHELL = hash("Shell");
	OVERRIDESHELL = hash("OverrideShell");
	WMSHELL = hash("WMShell");
	VENDORSHELL = hash("VendorShell");
	TRANSIENTSHELL = hash("TransientShell");
	TOPLEVELSHELL = hash("TopLevelShell");
	APPLICATIONSHELL = hash("ApplicationShell");
	CONSTRAINT = hash("Constraint");
	XMPRIMITIVE = hash("Xm/XmPrimitive");
	PRIMITIVE = hash("XmPrimitive");
	XMMANAGER = hash("Xm/Manager");
	MANAGER = hash("XmManager");
	DOLLAR = hash("$");
	action_proto.decl = malloc(sizeof(struct _Decl));
	action_proto.decl->type = hash("void");
	action_proto.decl->params = malloc(sizeof(struct _Decl));
	action_proto.decl->params->type = 0;
	action_proto.decl->params->name = DOLLAR;
	action_proto.decl->params->suffix = 0;
	action_proto.decl->params->next = malloc(sizeof(struct _Decl));
	action_proto.decl->params->next->type = hash("XEvent*");
	action_proto.decl->params->next->name = hash("event");
	action_proto.decl->params->next->suffix = 0;
	action_proto.decl->params->next->next = malloc(sizeof(struct _Decl));
	action_proto.decl->params->next->next->type = hash("String*");;
	action_proto.decl->params->next->next->name = hash("params");
	action_proto.decl->params->next->next->suffix = 0;
	action_proto.decl->params->next->next->next =
		malloc(sizeof(struct _Decl));
	action_proto.decl->params->next->next->next->type = hash("Cardinal*");
	action_proto.decl->params->next->next->next->name = hash("num_params");
	action_proto.decl->params->next->next->next->suffix = 0;
	action_proto.decl->params->next->next->next->next = NULL;
}
