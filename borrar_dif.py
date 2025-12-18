import os

def borrar_dif_files(root_dir):
    for dirpath, dirnames, filenames in os.walk(root_dir):
        for filename in filenames:
            if ("__DIF_" in filename) and (filename.lower().endswith('.txt') or filename.lower().endswith('.abap')):
                file_path = os.path.join(dirpath, filename)
                try:
                    os.remove(file_path)
                    print(f"Borrado: {file_path}")
                except Exception as e:
                    print(f"Error al borrar {file_path}: {e}")

if __name__ == "__main__":
    root_directory = os.path.dirname(os.path.abspath(__file__))
    borrar_dif_files(root_directory)