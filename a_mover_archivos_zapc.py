import os
import shutil

directorio_origen = '.'

def mover_archivo(origen, destino):
    directorio = os.path.dirname(destino)
    if not os.path.exists(directorio):
        os.makedirs(directorio)
    shutil.move(origen, destino)
    print(f'Movido: {os.path.basename(origen)} -> {directorio}/')

for archivo in os.listdir(directorio_origen):
    ruta_archivo = os.path.join(directorio_origen, archivo)
    if not os.path.isfile(ruta_archivo):
        continue  # Solo archivos, no directorios

    # Solo procesar archivos, no directorios, y que tengan extensión .txt
    if not os.path.isfile(ruta_archivo) or not ( archivo.lower().endswith('.txt') or archivo.lower().endswith('.slnk') ):
        continue
        
    # Solo si el nombre tiene al menos 4 caracteres
    if len(archivo) < 4:
        continue

    prefijo = archivo[:4]

    if "DOMA_" in archivo or "DTEL_" in archivo or "TABL_" in archivo or "TTYP_" in archivo:
        destino = os.path.join('', 'DDIC', prefijo, archivo)
        mover_archivo(ruta_archivo, destino)
    # Para cualquier otro archivo, NO crear directorios ni mover