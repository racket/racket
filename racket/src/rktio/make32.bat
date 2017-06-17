@echo off
echo ============================================
echo Assumes that the "librktio" project is built
echo ============================================
cl /I..\worksp\librktio demo.c ..\worksp\librktio\x32\Release\librktio.lib ws2_32.lib user32.lib shell32.lib
