SET ROPTS=--no-save --no-environ --no-init-file --no-restore --no-Rconsole

start /b R-Portable\App\R-Portable\bin\Rscript.exe runShinyApp.R
timeout 10
GoogleChromePortable\App\Chrome-bin\chrome.exe --app=http://127.0.0.1:7777
