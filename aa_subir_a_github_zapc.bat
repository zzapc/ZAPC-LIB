@echo off
:: Script para subir un directorio a GitHub en Windows 11

:: Solicitar datos al usuario
set /p COMMIT_MSG=Introduce el mensaje del commit:

:: Inicializar repositorio Git si no existe
if not exist ".git" (
    git init
)

:: Agregar todos los archivos
git add .

:: Hacer commit
git commit -m "%COMMIT_MSG%"

:: Agregar repositorio remoto
git remote remove origin 2>nul
git remote add origin https://github.com/zzapc/ZAPC-LIB.git

:: Subir a la rama main
git branch -M main
# Reescribe el historial para eliminar el archivo con el secreto
git filter-branch --force --index-filter \
  "git rm --cached --ignore-unmatch DOC/MG_SAP4.JSON" \
  --prune-empty --tag-name-filter cat -- --all

git push -u origin main --force --all



 


pause
