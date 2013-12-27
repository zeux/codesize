@echo off
call :build x86
call :build x64
goto :eof

:build
setlocal
%WINDIR%\Microsoft.NET\Framework\v4.0.30319\msbuild /nologo /v:minimal /p:VisualStudioVersion=12.0 /p:Configuration=Release /p:Platform=%1 codesize.fsproj
cd bin\Release_%1
..\..\sdks\7z\7zr a -sfx..\..\codesize.sfx ..\..\codesize_%1.exe *
goto :eof
