@echo off
echo ========================================
echo Your SSH Public Key
echo ========================================
echo.
type %USERPROFILE%\.ssh\id_ed25519.pub
echo.
echo ========================================
echo.
echo INSTRUCTIONS:
echo 1. Copy the key above (all of it!)
echo 2. SSH to server: ssh razinka@laguna.ku.lt
echo 3. On server run: mkdir -p ~/.ssh ^&^& chmod 700 ~/.ssh
echo 4. On server run: nano ~/.ssh/authorized_keys
echo 5. Paste the key, save (Ctrl+X, Y, Enter)
echo 6. On server run: chmod 600 ~/.ssh/authorized_keys
echo 7. On server run: exit
echo 8. Test: ssh razinka@laguna.ku.lt "echo Success"
echo.
pause
