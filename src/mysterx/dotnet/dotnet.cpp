// dotnet.cpp : Implementation of DLL Exports.

#include "stdafx.h"
#include "resource.h"

// The module attribute causes DllMain, DllRegisterServer and DllUnregisterServer to be automatically implemented for you
[ module(dll, uuid = "{D89BBDEE-CA88-413D-B013-9A608E69FDA2}",
		 name = "DotnetProfiler",
		 helpstring = "MysterX Dotnet 1.0 Type Library",
		 resource_name = "IDR_DOTNET") ];
