import pandas as pd
import time
import os
from playwright.sync_api import sync_playwright

input_file = r'c:\Proyectos\Denuncias\planilla.xlsx'
output_file = r'c:\Proyectos\Denuncias\planilla_actualizada.xlsx'

def extract_data_from_page(page):
    """
    Intenta extraer los datos del expediente una vez en la página de resultados.
    Como no conocemos el DOM exacto, usamos heurística sobre el texto.
    """
    try:
        # Esperar un momento a que los datos rendericen después del click en buscar
        page.wait_for_timeout(2000) 
        text_content = page.locator("body").inner_text()
        
        lines = [line.strip() for line in text_content.split('\n') if line.strip()]
        estado = ""
        asunto = ""
        fecha = ""
        
        for i, line in enumerate(lines):
            line_upper = line.upper()
            if ("ESTADO" == line_upper or "ESTADO:" in line_upper) and not estado:
                estado = lines[i+1] if i+1 < len(lines) else line
            if ("ASUNTO" == line_upper or "ASUNTO:" in line_upper) and not asunto:
                asunto = lines[i+1] if i+1 < len(lines) else line
            if ("FECHA" == line_upper or "FECHA:" in line_upper) and not fecha:
                fecha = lines[i+1] if i+1 < len(lines) else line
                
        return {
            "Estado": estado[:150], 
            "Asunto": asunto[:150],
            "Fecha": fecha[:150],
            "Texto Completo": text_content[:500] # Primeros 500 chars por si falla el parseo específico
        }
    except Exception as e:
        return {"Error": "Error al extraer: " + str(e)}

def main():
    print("==================================================")
    print("   EXTRACTOR DE EXPEDIENTES UTE (SEMI-AUTOMÁTICO)")
    print("==================================================")
    
    if not os.path.exists(input_file):
        print(f"Error: No se encuentra el archivo {input_file}")
        return
        
    print("Leyendo el archivo Excel...")
    try:
        df = pd.read_excel(input_file)
    except Exception as e:
        print(f"Error leyendo Excel: {e}")
        return
    
    # Buscar dinámicamente la columna de expediente
    exp_col = next((col for col in df.columns if 'expediente' in str(col).lower()), None)
    if not exp_col:
        print("No se encontró una columna que contenga la palabra 'expediente' en la primera fila.")
        print("Columnas disponibles:", df.columns.tolist())
        return
        
    print(f"Columna de expedientes detectada: '{exp_col}'")
    
    # Agregar columnas si no existen
    nuevas_columnas = ['Exp_Estado', 'Exp_Asunto', 'Exp_Fecha', 'Exp_TextoExtracto']
    for col in nuevas_columnas:
        if col not in df.columns:
            df[col] = ''
            
    total_filas = len(df)
    procesados = 0
    actualizados = 0
    
    with sync_playwright() as p:
        print("Abriendo navegador Chrome...")
        # Lanza el navegador de forma visible
        browser = p.chromium.launch(headless=False)
        context = browser.new_context(viewport={"width": 1280, "height": 800})
        page = context.new_page()
        
        for index, row in df.iterrows():
            exp_val = str(row[exp_col]).strip()
            
            # Formato estándar de expediente de UTE tiene "/", ej. 2024/36001/000408
            if '/' not in exp_val or len(exp_val) < 8:
                continue
                
            procesados += 1
            
            # Si ya tiene datos (para poder pausar y reanudar otro día)
            if pd.notna(row['Exp_Estado']) and str(row['Exp_Estado']).strip() != '':
                print(f"Saltando {exp_val} (ya procesado).")
                continue
                
            print(f"\n---> Expediente {procesados}/{total_filas}: [{exp_val}]")
            
            try:
                page.goto("https://expediente.ute.com.uy/ConsultasWebMMA/")
                
                # Insertar valor
                page.wait_for_selector("input#NumeroExpediente", timeout=15000)
                page.fill("input#NumeroExpediente", exp_val)
                
                # Instrucciones para el usuario humano
                print("  =======================================================")
                print("  1. Por favor, resuelve el ReCAPTCHA ('No soy un robot').")
                print("  2. Inicia la búsqueda (clic en el botón).")
                print("  3. Espera a ver los resultados en pantalla.")
                input("  >>> PRESIONA [ENTER] AQUÍ CUANDO VEAS LOS RESULTADOS <<< ")
                print("  =======================================================")
                
                # Extraemos datos
                print("  Extrayendo datos de la página...")
                datos = extract_data_from_page(page)
                
                # Actualizar Dataframe
                df.at[index, 'Exp_Estado'] = str(datos.get('Estado', ''))
                df.at[index, 'Exp_Asunto'] = str(datos.get('Asunto', ''))
                df.at[index, 'Exp_Fecha'] = str(datos.get('Fecha', ''))
                
                txt_limpio = str(datos.get('Texto Completo', datos.get('Error', ''))).replace('\n', ' | ')
                df.at[index, 'Exp_TextoExtracto'] = txt_limpio
                
                # Guardamos después de cada consulta exitosa
                df.to_excel(output_file, index=False)
                actualizados += 1
                
                print(f"  OK. Guardado. (Estado: {datos.get('Estado', '')})")
                
            except Exception as e:
                print(f"  ERROR al procesar {exp_val}: {e}")
                
        print("\n==================================================")
        print(f"PROCESO FINALIZADO.")
        print(f"Total expedientes actualizados: {actualizados}")
        print(f"Archivo guardado en: {output_file}")
        print("==================================================")
        
        browser.close()

if __name__ == "__main__":
    main()
