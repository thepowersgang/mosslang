echo on
cargo run -- .\example_code\hello_world.ms > 1.txt
@if %errorlevel% neq 0 exit /b %errorlevel%
call "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\bin\amd64\vcvars64.bat"
@if %errorlevel% neq 0 exit /b %errorlevel%
link.exe /DEBUG /out:.\example_code\hello_world.exe /SUBSYSTEM:CONSOLE .\example_code\hello_world.obj libcmt.lib
@if %errorlevel% neq 0 exit /b %errorlevel%