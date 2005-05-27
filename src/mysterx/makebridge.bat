call CSC /target:exe /o- /debug:full /r:"C:\Program Files\Office PIAs\Microsoft.Office.Interop.Excel.dll" /d:STRONG bridge.cs /unsafe
copy /y bridge.exe ..\..
copy /y bridge.pdb ..\..

