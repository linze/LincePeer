#!/bin/bash
echo "[*] Lanzando "gnatmake" sobre "lince.adb"  suponiendo librerias en "/opt/ll/lib"."
gnatmake -I/opt/ll/lib lince.adb
gnatmake -I/opt/ll/lib lince.adb
echo "[*] Limpiando la casa."
rm *.ali
rm *.o
echo "[*] Fin del proceso. Aprende a usar makefile algún día, carapan ;)"

