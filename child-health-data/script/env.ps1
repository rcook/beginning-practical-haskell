$icuBinPathFile = Join-Path $PSScriptRoot ..\.stack-work\icu-bin-path.txt

if (Test-Path $icuBinPathFile) {
  $icuBinPath = (Get-Content -Raw $icuBinPathFile).Trim()
}
else {
  pushd $PSScriptRoot
  stack exec -- sh get-icu-bin-path.sh > $icuBinPathFile
  popd
}

if (-not ($env:PATH -match [Regex]::Escape($icuBinPath))) {
  $env:PATH = $icuBinPath + ";" + $env:PATH
}
