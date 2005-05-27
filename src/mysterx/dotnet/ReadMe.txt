This directory contains an important bug fix for the .NET engine.

For details about the build process, see `overview.txt' within this
directory.

The .NET engine has a race condition within the thread library.  This
directory contains the code that eliminates the race condition.  This
library pretends to be a profiler for the .NET runtime.  It intercepts
thread activity and compilation activity through profiling hooks
provided by the .NET runtime.  It tracks the threads as they enter and
leave the CLR, and patches the JIT compiler to add special code around
the Thread.Abort() method.

To use this code, the environment variable `Cor_Enable_Profiling' must
be set to "1", and the environment variable `Cor_Profiler' must be set
to "MysterX.DotnetProfiler" before starting the CLR.  (They may be set
by calling the MzScheme function putenv.)

If correctly set, a message like the following will be printed when
the CLR is started:

    Initializing DotNet synchronization patch.
    Built at 13:56:09 on Jul 17 2003

This message will not be printed if the environment variables are not
set, or if the dotnet module is not compiled and registered properly.

