       *> *************************************************************
       *> UNIVERSIDAD NACIONAL ABIERTA                                *
       *> TRABAJO PRÁCTICO                                            *
       *> ASIGNATURA: PROCESAMIENTO DE DATOS                         *
       *> CÓDIGO: 330                                                 *
       *> COMPILADO CON GnuCOBOL VERSIÓN (3.1.2.0)  GNU/LINUX         *
       *> EDITOR DE CÓDIGO: VISUAL STUDIO CODE                        *
       *> *************************************************************
       *> PARA COMPILAR: cobc -O restaurante.cbl -x -o main           *
       *> *************************************************************
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RESTAURANTE.
       AUTHOR. JOSE GONZALES.
       DATE-WRITTEN. [18-02-2025].
       DATE-COMPILED. [25-03-2025].
      *>  ------------------------------------------------------------- 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       *>  ASIGNACIÓN DE LOS ARCHIVOS LÓGICOS AL ARCHIVO FÍSICO:
       *>  ARCHIVO PARA LA CARTA DEL RESTAURANTE
           SELECT CLIENTES ASSIGN TO "CLIENTES.TXT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CLIENTE-RIF
                  FILE STATUS IS FS-CLIENTES.
       
           SELECT VENTAS ASSIGN TO "VENTAS.TXT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS VENTA-FACTURA
                  FILE STATUS IS FS-VENTAS.
       
           SELECT FACTURAS ASSIGN TO "FACTURAS.TXT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS FACTURA-RIF
                  FILE STATUS IS FS-FACTURAS.
      *>  ------------------------------------------------------------- 
       DATA DIVISION.
       FILE SECTION.
       FD CLIENTES.
       01 CLIENTE-REG.
          05 CLIENTE-RIF        PIC X(10).
          05 CLIENTE-RAZON      PIC X(50).
          05 CLIENTE-NOMBRE     PIC X(50).
          05 CLIENTE-TELEFONO   PIC X(15).
          05 CLIENTE-CELULAR    PIC X(15).
          05 CLIENTE-EMAIL      PIC X(50).
          05 CLIENTE-DIRECCION  PIC X(100).

       FD VENTAS.
       01 VENTA-REG.
          05 VENTA-CEDULA       PIC X(10).
          05 VENTA-FACTURA      PIC X(10).
          05 VENTA-FECHA        PIC X(10).
          05 VENTA-CANTIDAD     PIC 9(5).
          05 VENTA-CATEGORIA    PIC X(20).

       FD FACTURAS.
       01 FACTURA-REG.
          05 FACTURA-RIF        PIC X(10).
          05 FACTURA-NUMERO     PIC X(10).
          05 FACTURA-RAZON      PIC X(50).
          05 FACTURA-PRODUCTO   PIC X(50).
          05 FACTURA-PRECIO     PIC 9(7)V99.
          05 FACTURA-DIRECCION  PIC X(100).
          05 FACTURA-TELEFONOS  PIC X(30).
          05 FACTURA-IVA        PIC 9(5)V99.
          05 FACTURA-TOTAL      PIC 9(7)V99.
      *>  ------------------------------------------------------------- 
       WORKING-STORAGE SECTION.
       01 FS-CLIENTES           PIC XX.
       01 FS-VENTAS             PIC XX.
       01 FS-FACTURAS           PIC XX.
       01 WS-WS-OPCION                PIC 9.
       01 SALIR                 PIC X VALUE 'N'.
       01 VALIDO                PIC X VALUE 'N'.
       01 VALIDA                PIC X VALUE 'N'.
       01 CONFIRMAR             PIC X.
      *>  ------------------------------------------------------------- 
       *> Manejo de Fecha
       77 WS-FECHA-SISTEMA            PIC 9(06).
       01 WS-FECHA-FORMATO.
          02 WS-FECHA-ANNO           PIC 9(02).
          02 WS-FECHA-MES            PIC 9(02).
          02 WS-FECHA-DIA            PIC 9(02).
       77 WS-ANNO-EXTENDIDO           PIC 9(04).
      *>  ------------------------------------------------------------- 
       01 WS-MES                  PIC 99.
       01 WS-ANNO                 PIC 9999.
       01 WS-MES-FACTURA          PIC 99.
       01 WS-ANNO-FACTURA         PIC 9999.
       01 WS-MES-VENTA            PIC 99.
       01 WS-ANNO-VENTA           PIC 9999.
      *>  ------------------------------------------------------------- 
       01 WS-FECHA-LEGIBLE.
          05 WS-FECHA-DIA    PIC 9(02).
          05 FILLER          PIC X(01) VALUE '/'.
          05 WS-FECHA-MES    PIC 9(02).
          05 FILLER          PIC X(01) VALUE '/'.
          05 WS-FECHA-ANNO   PIC 9(04).
      *>  ------------------------------------------------------------- 
       01 WS-ACUMULADORES.
          05 WS-TOTAL-VENTAS        PIC 9(10)V99 VALUE ZERO.
          05 WS-CANTIDAD-PRODUCTOS  PIC 9(10)    VALUE ZERO.
      *>  ------------------------------------------------------------- 
       01 WS-CONTADORES.
          05 WS-CONTADOR-FACTURAS   PIC 9(10)    VALUE ZERO.
          05 WS-CONTADOR-CLIENTES   PIC 9(10)    VALUE ZERO.
       *>  -------------------------------------------------------------    
       *>  VARIABLES DE USO GENERAL 
       77  WS-FIN                      PIC 9(01) VALUE ZERO.
       77  WS-FIN-ARCHIVO              PIC 9(01) VALUE ZERO.
       77  WS-ENCONTRADO               PIC 9(01) VALUE ZERO.
       77  WS-OPCION                   PIC 9(01).
       77  WS-FIN-SUBMENU              PIC 9(01) VALUE ZERO.
       77  WS-OPCION-SUBMENU           PIC 9(01).
       77  WS-CONSULTA                 PIC 9(01) VALUE ZERO.
       77  WS-TOTAL-FACTURAS           PIC 9(01) VALUE ZERO.
       
       
       PROCEDURE DIVISION.

       MAIN-PROGRAM.
           MOVE 0 TO WS-FIN

           PERFORM 000-MENU-PRINCIPAL UNTIL WS-FIN = 1.

           DISPLAY "CERRANDO: SISTEMA RESTAURANTE..."
           STOP RUN.

       *> MENUS
       000-MENU-PRINCIPAL.
           DISPLAY "++++++++++++++++++++++++++++++++++++++++++++++".
           DISPLAY "+      SISTEMA DE GESTIÓN DEL RESTAURANTE    +".
           DISPLAY "++++++++++++++++++++++++++++++++++++++++++++++".
           
           MOVE 0 TO WS-FIN-SUBMENU

           ACCEPT WS-FECHA-SISTEMA FROM DATE.
           MOVE WS-FECHA-SISTEMA TO WS-FECHA-FORMATO.

           COMPUTE WS-ANNO-EXTENDIDO = WS-FECHA-ANNO 
                   OF WS-FECHA-FORMATO + 2000.

           DISPLAY "BIENVENIDO | FECHA: "
                   WS-FECHA-DIA OF WS-FECHA-FORMATO "/" 
                   WS-FECHA-MES OF WS-FECHA-FORMATO "/" 
                   WS-ANNO-EXTENDIDO.
           DISPLAY " ".
           DISPLAY "MENÚ PRINCIPAL".
           DISPLAY "----------------------------------------------".
           DISPLAY "1 - GESTIÓN DE CLIENTES".
           DISPLAY "2 - GESTIÓN DE VENTAS".
           DISPLAY "3 - GESTIÓN DE FACTURAS".
           DISPLAY "4 - INFORMES Y REPORTES".
           DISPLAY "----------------------------------------------".
           DISPLAY "5 - SALIR DEL SISTEMA".
           DISPLAY " ".
           DISPLAY "SELECCIONE UNA OPCIÓN: " WITH NO ADVANCING.

           ACCEPT WS-OPCION.
           
           EVALUATE WS-OPCION
           WHEN 1 PERFORM     001-SUBMENU-GESTION-CLIENTES
           WHEN 2 PERFORM     001-SUBMENU-GESTION-VENTAS
           WHEN 3 PERFORM     001-SUBMENU-GESTION-FACTURAS
           WHEN 4 PERFORM     001-SUBMENU-GENERAR-INFORMES
                            UNTIL WS-FIN-SUBMENU = 1
               WHEN 9     MOVE 1 TO WS-FIN
               WHEN OTHER
                   DISPLAY " "
                   DISPLAY "OPCION INVÁLIDA, VERIFICA"
           END-EVALUATE.

       *> PROCEDIMIENTOS COMUNES
       *>  -------------------------------------------------------------
       000-ABRIR-ARCHIVO-CLIENTES.
           OPEN I-O CLIENTES.
           IF FS-CLIENTES NOT = "00"
              DISPLAY "Error al abrir el archivo. Código: " FS-CLIENTES
              PERFORM 000-REINTENTAR-OPERACION
           ELSE
              DISPLAY "Archivo de clientes abierto correctamente."
           END-IF.

       000-CERRAR-ARCHIVO-CLIENTES.
           CLOSE CLIENTES.
           IF FS-CLIENTES NOT = "00"
              DISPLAY "Error al abrir el archivo. Código: " FS-CLIENTES
           ELSE
              DISPLAY "Archivo de clientes cerrado correctamente."
           END-IF.

       000-ABRIR-ARCHIVO-VENTAS.
           OPEN I-O VENTAS.
           IF FS-VENTAS NOT = "00"
              DISPLAY "Error al abrir el archivo. Código: " FS-VENTAS
              PERFORM 000-REINTENTAR-OPERACION
           END-IF.

       000-CERRAR-ARCHIVO-VENTAS.
           CLOSE VENTAS.

       000-ABRIR-ARCHIVO-FACTURAS.
           OPEN I-O FACTURAS.
           IF FS-FACTURAS NOT = "00"
              DISPLAY "Error al abrir el archivo. Código: " FS-FACTURAS
              PERFORM 000-REINTENTAR-OPERACION
           END-IF.

       000-CERRAR-ARCHIVO-FACTURAS.
           CLOSE FACTURAS.

       000-REINTENTAR-OPERACION.
           DISPLAY "¿Reintentar? (S/N): " WITH NO ADVANCING
           ACCEPT CONFIRMAR.
           IF CONFIRMAR = "S" OR "s"
              PERFORM MAIN-PROGRAM
           ELSE
              DISPLAY "Operación cancelada. "
              PERFORM 000-MENU-PRINCIPAL
           END-IF.

       *> GESTIÓN DE CLIENTES
       *>  -------------------------------------------------------------

       001-SUBMENU-GESTION-CLIENTES.
           DISPLAY "++++++++++++++++++++++++++++++++++++++++++++++".
           DISPLAY "+          GESTIÓN DE CLIENTES               +".
           DISPLAY "++++++++++++++++++++++++++++++++++++++++++++++".
           DISPLAY " ".
           DISPLAY "1 - REGISTRAR/MODIFICAR CLIENTE".
           DISPLAY "2 - ELIMINAR CLIENTE".
           DISPLAY "----------------------------------------------".
           DISPLAY "4 - VOLVER AL MENÚ PRINCIPAL".
           DISPLAY " ".
           DISPLAY "SELECCIONE UNA OPCIÓN: " WITH NO ADVANCING.
           ACCEPT WS-OPCION.
           EVALUATE WS-OPCION
           WHEN 1 PERFORM     002-REGISTRAR-CLIENTE
           WHEN 2 PERFORM     002-ELIMINAR-CLIENTE
           WHEN 4 PERFORM     000-MENU-PRINCIPAL
           WHEN OTHER
              DISPLAY " "
              DISPLAY "OPCION INVÁLIDA, VERIFICA"
           END-EVALUATE.

       002-REGISTRAR-CLIENTE.
                DISPLAY " ".
                DISPLAY "REGISTRO Y/O ACTUALIZACIÓN DE CLIENTES.".
                DISPLAY "---------------------------------------".
                DISPLAY " ".
       
                DISPLAY "RIF DEL CLIENTE (Formato: V-1234789): " 
                    WITH NO ADVANCING.
                ACCEPT CLIENTE-RIF.
       
                PERFORM 000-ABRIR-ARCHIVO-CLIENTES.
                    MOVE CLIENTE-RIF TO CLIENTE-RIF.
           
                    READ CLIENTES
                        KEY IS CLIENTE-RIF
                        INVALID KEY 
                            MOVE 0 TO WS-CONSULTA
                        NOT INVALID KEY 
                            MOVE 1 TO WS-CONSULTA.
                PERFORM 000-CERRAR-ARCHIVO-CLIENTES.
       
                IF WS-CONSULTA = 1
                     DISPLAY " "
                     DISPLAY "+++ADVERTENCIA: CLIENTE YA EXISTE, "
                                 "LOS DATOS QUE INTRODUZCA "
                                 "ACTUALIZARAN LOS YA EXISTENTES+++"
                     DISPLAY " "
                     MOVE ZERO TO WS-CONSULTA
                END-IF.
            *>  ENTRADA DE DATOS
                DISPLAY "RAZÓN SOCIAL: " WITH NO ADVANCING.
                ACCEPT CLIENTE-RAZON.
                DISPLAY "NOMBRE DEL CLIENTE: " WITH NO ADVANCING.
                ACCEPT CLIENTE-NOMBRE.
                DISPLAY "TELÉFONO FIJO: " WITH NO ADVANCING.
                ACCEPT CLIENTE-TELEFONO.
                DISPLAY "TELÉFONO CELULAR: " WITH NO ADVANCING.
                ACCEPT CLIENTE-CELULAR.
                DISPLAY "CORREO ELECTRÓNICO: " WITH NO ADVANCING.
                ACCEPT CLIENTE-EMAIL.
                DISPLAY "DIRECCIÓN: " WITH NO ADVANCING.
                ACCEPT CLIENTE-DIRECCION.
            *>  REGISTRO DE LOS DATOS EN EL ARCHIVO
                PERFORM 000-ABRIR-ARCHIVO-CLIENTES.
                IF WS-CONSULTA = 0
                    WRITE CLIENTE-REG
                        INVALID KEY
                            DISPLAY "Error al registrar el cliente. "
                            "Código: " FS-CLIENTES
                        NOT INVALID KEY
                            DISPLAY "Cliente registrado exitosamente."
                    END-WRITE
                ELSE
                    REWRITE CLIENTE-REG
                        INVALID KEY
                            DISPLAY "Error al actualizar el cliente. "
                            "Código: " FS-CLIENTES
                        NOT INVALID KEY
                            DISPLAY "Cliente actualizado exitosamente."
                    END-REWRITE
                END-IF.
                PERFORM 000-CERRAR-ARCHIVO-CLIENTES.
                DISPLAY "OPERACIÓN COMPLETADA... " WITH NO ADVANCING.
                STOP "ENTER PARA CONTINUAR".

       002-ELIMINAR-CLIENTE.
                DISPLAY " ".
                DISPLAY "ELIMINACIÓN DE CLIENTES.".
                DISPLAY "---------------------------------------".
                DISPLAY " ".
       
                DISPLAY "RIF DEL CLIENTE (Formato: V-1234789): " 
                    WITH NO ADVANCING.
                ACCEPT CLIENTE-RIF.
       
                PERFORM 000-ABRIR-ARCHIVO-CLIENTES.
                    MOVE CLIENTE-RIF TO CLIENTE-RIF.
           
                    READ CLIENTES
                        KEY IS CLIENTE-RIF
                        INVALID KEY 
                            MOVE 0 TO WS-CONSULTA
                        NOT INVALID KEY 
                            MOVE 1 TO WS-CONSULTA.
                PERFORM 000-CERRAR-ARCHIVO-CLIENTES.
       
                IF WS-CONSULTA = 1
                     DISPLAY " "
                     DISPLAY "+++ADVERTENCIA: CLIENTE ENCONTRADO. "
                                 "¿DESEA ELIMINARLO?+++"
                     DISPLAY " "
                     DISPLAY "¿Está seguro de eliminar al "
                             "cliente " CLIENTE-RAZON "? (S/N): "
                     ACCEPT CONFIRMAR

                     IF CONFIRMAR = "S" OR CONFIRMAR = "s"
                        PERFORM 000-ABRIR-ARCHIVO-CLIENTES
                        DELETE CLIENTES
                            INVALID KEY
                                DISPLAY "Error al eliminar el cliente. "
                                        "Código: " FS-CLIENTES
                            NOT INVALID KEY
                                DISPLAY "Cliente eliminado exitosamente"
                        END-DELETE
                        PERFORM 000-CERRAR-ARCHIVO-CLIENTES
                     ELSE
                        DISPLAY "Eliminación cancelada."
                     END-IF
                ELSE
                    DISPLAY "Cliente no encontrado."
                END-IF.
       
                DISPLAY "OPERACIÓN COMPLETADA... " WITH NO ADVANCING.
                STOP "ENTER PARA CONTINUAR".

       *> GESTIÓN DE VENTAS

       001-SUBMENU-GESTION-VENTAS.
           DISPLAY "++++++++++++++++++++++++++++++++++++++++++++++".
           DISPLAY "+          GESTIÓN DE VENTAS                 +".
           DISPLAY "++++++++++++++++++++++++++++++++++++++++++++++".
           DISPLAY " ".
           DISPLAY "1 - REGISTRAR /MODIFICAR VENTA".
           DISPLAY "2 - CONSULTAR VENTA".
           DISPLAY "3 - ELIMINAR VENTA".
           DISPLAY "----------------------------------------------".
           DISPLAY "4 - VOLVER AL MENÚ PRINCIPAL".
           DISPLAY " ".
           DISPLAY "SELECCIONE UNA OPCIÓN: " WITH NO ADVANCING.
           ACCEPT WS-OPCION.
           EVALUATE WS-OPCION
           WHEN 1 PERFORM     002-REGISTRAR-VENTA
           WHEN 2 PERFORM     002-CONSULTAR-VENTA
           WHEN 3 PERFORM     002-ELIMINAR-VENTA
           WHEN 4 PERFORM     000-MENU-PRINCIPAL
           WHEN OTHER
              DISPLAY " "
              DISPLAY "OPCION INVÁLIDA, VERIFICA"
           END-EVALUATE.

       002-REGISTRAR-VENTA.
                  DISPLAY " ".
                  DISPLAY "REGISTRO DE VENTAS.".
                  DISPLAY "--------------------------------------".
                  DISPLAY " ".
       
                  DISPLAY "CÉDULA DEL CLIENTE (Formato: 1234567890): " 
                      WITH NO ADVANCING.
                  ACCEPT VENTA-CEDULA.
       
                  PERFORM 000-ABRIR-ARCHIVO-VENTAS.
                      MOVE VENTA-CEDULA TO VENTA-CEDULA.
       
                      READ VENTAS
                          KEY IS VENTA-CEDULA
                          INVALID KEY 
                              MOVE 0 TO WS-CONSULTA
                          NOT INVALID KEY 
                              MOVE 1 TO WS-CONSULTA.
                  PERFORM 000-CERRAR-ARCHIVO-VENTAS.
       
                  IF WS-CONSULTA = 1
                       DISPLAY " "
                       DISPLAY "+++ADVERTENCIA: VENTA YA REGISTRADA, "
                                   "LOS DATOS QUE INTRODUZCA "
                                   "ACTUALIZARAN LOS YA EXISTENTES+++"
                       DISPLAY " "
                       MOVE ZERO TO WS-CONSULTA
                  END-IF.
       
              *>  ENTRADA DE DATOS
                  DISPLAY "NÚMERO DE FACTURA: " WITH NO ADVANCING.
                  ACCEPT VENTA-FACTURA.
       
                  DISPLAY "FECHA (DD/MM/AAAA): " WITH NO ADVANCING.
                  ACCEPT VENTA-FECHA.
       
                  DISPLAY "CANTIDAD VENDIDA: " WITH NO ADVANCING.
                  ACCEPT VENTA-CANTIDAD.
       
                  DISPLAY "CATEGORÍA DEL PRODUCTO: " WITH NO ADVANCING.
                  ACCEPT VENTA-CATEGORIA.
       
              *>  REGISTRO DE LOS DATOS EN EL ARCHIVO
                  PERFORM 000-ABRIR-ARCHIVO-VENTAS.
                  IF WS-CONSULTA = 0
                      WRITE VENTA-REG
                          INVALID KEY
                              DISPLAY "Error al registrar la venta. "
                                      "Código: " FS-VENTAS
                          NOT INVALID KEY
                              DISPLAY "Venta registrada exitosamente."
                      END-WRITE
                  ELSE
                      REWRITE VENTA-REG
                          INVALID KEY
                              DISPLAY "Error al actualizar la venta. "
                                      "Código: " FS-VENTAS
                          NOT INVALID KEY
                              DISPLAY "Venta actualizada exitosamente."
                      END-REWRITE
                  END-IF.
                  PERFORM 000-CERRAR-ARCHIVO-VENTAS.
       
                  DISPLAY "OPERACIÓN COMPLETADA... " WITH NO ADVANCING.
                  STOP "ENTER PARA CONTINUAR".
           
       002-CONSULTAR-VENTA.
           PERFORM 000-ABRIR-ARCHIVO-VENTAS.
           DISPLAY "Ingrese Número de Factura a consultar: "
           ACCEPT VENTA-FACTURA.
           READ VENTAS
               INVALID KEY
                   DISPLAY "Venta no encontrada."
               NOT INVALID KEY
                   DISPLAY "Cédula: " VENTA-CEDULA
                   DISPLAY "Fecha: " VENTA-FECHA
                   DISPLAY "Cantidad: " VENTA-CANTIDAD
                   DISPLAY "Categoría: " VENTA-CATEGORIA
           END-READ.
           PERFORM 000-CERRAR-ARCHIVO-VENTAS.

       002-ELIMINAR-VENTA.
                DISPLAY " ".
                DISPLAY "ELIMINACIÓN DE VENTAS.".
                DISPLAY "---------------------------------------".
                DISPLAY " ".
      
                DISPLAY "NÚMERO DE FACTURA: " WITH NO ADVANCING.
                ACCEPT VENTA-FACTURA.
      
                PERFORM 000-ABRIR-ARCHIVO-VENTAS.
                    MOVE VENTA-FACTURA TO VENTA-FACTURA.
      
                    READ VENTAS
                        KEY IS VENTA-FACTURA
                        INVALID KEY 
                            MOVE 0 TO WS-CONSULTA
                        NOT INVALID KEY 
                            MOVE 1 TO WS-CONSULTA.
                PERFORM 000-CERRAR-ARCHIVO-VENTAS.
      
                IF WS-CONSULTA = 1
                     DISPLAY " "
                     DISPLAY "+++ADVERTENCIA: VENTA ENCONTRADA. "
                                 "¿DESEA ELIMINARLA?+++"
                     DISPLAY " "
                     DISPLAY "¿Está seguro de eliminar la venta con "
                             "número de factura " VENTA-FACTURA 
                             "? (S/N): " WITH NO ADVANCING.
                     ACCEPT CONFIRMAR.
      
                     IF CONFIRMAR = "S" OR CONFIRMAR = "s"
                         PERFORM 000-ABRIR-ARCHIVO-VENTAS
                         DELETE VENTAS
                             INVALID KEY
                                 DISPLAY "Error al eliminar la venta. "
                                         "Código: " FS-VENTAS
                             NOT INVALID KEY
                                 DISPLAY "Venta eliminada exitosamente."
                         END-DELETE
                         PERFORM 000-CERRAR-ARCHIVO-VENTAS
                     ELSE
                         DISPLAY "Eliminación cancelada."
                     END-IF.
                DISPLAY "OPERACIÓN COMPLETADA... " WITH NO ADVANCING.
                STOP "ENTER PARA CONTINUAR".

       *> GESTIÓN DE FACTURAS

       001-SUBMENU-GESTION-FACTURAS.
           DISPLAY "++++++++++++++++++++++++++++++++++++++++++++++".
           DISPLAY "+          GESTIÓN DE FACTURAS               +".
           DISPLAY "++++++++++++++++++++++++++++++++++++++++++++++".
           DISPLAY " ".
           DISPLAY "1 - GENERAR FACTURA".
           DISPLAY "2 - CONSULTAR HISTORIAL DE FACTURAS".
           DISPLAY "----------------------------------------------".
           DISPLAY "3 - VOLVER AL MENÚ PRINCIPAL".
           DISPLAY " ".
           DISPLAY "SELECCIONE UNA OPCIÓN: " WITH NO ADVANCING.
           ACCEPT WS-OPCION.
           EVALUATE WS-OPCION
           WHEN 1 PERFORM     002-GENERAR-FACTURA
           WHEN 2 PERFORM     002-CONSULTAR-FACTURAS
           WHEN 3 PERFORM     000-MENU-PRINCIPAL
           WHEN OTHER
              DISPLAY " "
              DISPLAY "OPCION INVÁLIDA, VERIFICA"
           END-EVALUATE.
       
       002-GENERAR-FACTURA.
           PERFORM 000-ABRIR-ARCHIVO-FACTURAS.
           MOVE 'N' TO VALIDO.
           PERFORM UNTIL VALIDO = 'Y'
           DISPLAY "Ingrese Número de Factura (Formato: 123457890): "
                   WITH NO ADVANCING
           ACCEPT FACTURA-NUMERO
           DISPLAY "Ingrese RIF del cliente: " WITH NO ADVANCING
           ACCEPT FACTURA-RIF
           READ FACTURAS
              INVALID KEY
                 DISPLAY "Ingrese Razón Social del Cliente: " 
                         WITH NO ADVANCING
                 ACCEPT FACTURA-RAZON
                 DISPLAY "Ingrese Producto y Precio: " 
                         WITH NO ADVANCING
                 ACCEPT FACTURA-PRODUCTO
                 DISPLAY "Ingrese Dirección: " WITH NO ADVANCING
                 ACCEPT FACTURA-DIRECCION
                 DISPLAY "Ingrese Teléfonos: " WITH NO ADVANCING
                 ACCEPT FACTURA-TELEFONOS
                 DISPLAY "Ingrese IVA: " WITH NO ADVANCING
                 ACCEPT FACTURA-IVA
                 DISPLAY "Ingrese Monto Total: " WITH NO ADVANCING
                 ACCEPT FACTURA-TOTAL
                 DISPLAY "¿Seguro de generar la factura? (S/N): "
                         WITH NO ADVANCING
                 ACCEPT CONFIRMAR
                 IF CONFIRMAR = "S" OR "s"
                    WRITE FACTURA-REG
                       INVALID KEY
                          DISPLAY "Error al generar la factura."
                       NOT INVALID KEY
                          DISPLAY "Factura generada exitosamente."
                          MOVE 'Y' TO VALIDO
                    END-WRITE
                 ELSE
                    DISPLAY "Generación de factura cancelada."
                    MOVE 'Y' TO VALIDO
                 END-IF
              NOT INVALID KEY
                 DISPLAY "Error: Ya existe una factura con el mismo "
                         "RIF y número."
           END-READ
           END-PERFORM.
           PERFORM 000-CERRAR-ARCHIVO-FACTURAS.

       002-CONSULTAR-FACTURAS.
           PERFORM 000-ABRIR-ARCHIVO-FACTURAS.
           DISPLAY "Ingrese RIF del cliente: " WITH NO ADVANCING.
           ACCEPT FACTURA-RIF.
           
           MOVE 'N' TO VALIDO.
           PERFORM UNTIL VALIDO = 'Y'
               START FACTURAS KEY IS EQUAL TO FACTURA-RIF
                   INVALID KEY
                       DISPLAY "No se encontraron facturas del RIF: "
                               FACTURA-RIF
                       MOVE 'Y' TO VALIDO
                   NOT INVALID KEY
                       DISPLAY "Facturas encontradas para el RIF: "
                               FACTURA-RIF
                       DISPLAY "---------------------------------------"
                       PERFORM UNTIL FS-FACTURAS = "10"
                           READ FACTURAS NEXT RECORD
                               AT END
                                   MOVE "10" TO FS-FACTURAS
                               NOT AT END
                                   IF FACTURA-RIF = CLIENTE-RIF
                                       DISPLAY "Número de Factura: "
                                               FACTURA-NUMERO
                                       DISPLAY "Razón Social: "
                                               FACTURA-RAZON
                                       DISPLAY "Producto y Precio: "
                                               FACTURA-PRODUCTO " - "
                                               FACTURA-PRECIO
                                       DISPLAY "Dirección: "
                                               FACTURA-DIRECCION
                                       DISPLAY "Teléfonos: "
                                               FACTURA-TELEFONOS
                                       DISPLAY "IVA: " FACTURA-IVA
                                       DISPLAY "Monto Total: "
                                               FACTURA-TOTAL
                                       DISPLAY "-----------------------"
                                               "---------------------"
                                   END-IF
                           END-READ
                       END-PERFORM
                       MOVE 'Y' TO VALIDO
               END-START
           END-PERFORM.
           
           PERFORM 000-CERRAR-ARCHIVO-FACTURAS.
           DISPLAY "Consulta finalizada. Presione ENTER para continuar."
           ACCEPT CONFIRMAR.

       *> INFORMES Y REPORTES

       001-SUBMENU-GENERAR-INFORMES.
           DISPLAY "++++++++++++++++++++++++++++++++++++++++++++++".
           DISPLAY "+          INFORMES Y REPORTES               +".
           DISPLAY "++++++++++++++++++++++++++++++++++++++++++++++".
           DISPLAY " ".
           DISPLAY "1 - LISTADO MENSUAL DE CLIENTES".
           DISPLAY "2 - LISTADO MENSUAL DE FACTURAS".
           DISPLAY "3 - RESUMEN MENSUAL DE VENTAS".
           DISPLAY "----------------------------------------------".
           DISPLAY "4 - VOLVER AL MENÚ PRINCIPAL".
           DISPLAY " ".
           DISPLAY "SELECCIONE UNA OPCIÓN: " WITH NO ADVANCING.
           ACCEPT WS-OPCION.
           EVALUATE WS-OPCION
           WHEN 1 PERFORM     002-LISTADO-CLIENTES
           WHEN 2 PERFORM     002-LISTADO-FACTURAS
           WHEN 2 PERFORM     002-RESUMEN-VENTAS
           WHEN 3 PERFORM     000-MENU-PRINCIPAL
           WHEN OTHER
              DISPLAY " "
              DISPLAY "OPCION INVÁLIDA, VERIFICA"
           END-EVALUATE.

       002-LISTADO-CLIENTES.
           DISPLAY " "
           DISPLAY "********************************"
           DISPLAY "LISTADO MENSUAL DE CLIENTES: "
           DISPLAY "********************************"
           DISPLAY "INTRODUZCA EL MES (MM): " WITH NO ADVANCING.
           ACCEPT WS-MES.
           DISPLAY "INTRODUZCA EL AÑO (AAAA): " WITH NO ADVANCING.
           ACCEPT WS-ANNO.

           MOVE 0 TO WS-FIN-ARCHIVO.
           PERFORM 000-ABRIR-ARCHIVO-CLIENTES.
           PERFORM 004-LEE-SIG-CLIENTE.

           MOVE 0 TO WS-ENCONTRADO.
           DISPLAY " "
           DISPLAY "CLIENTES PARA EL MES: " WS-MES "/" WS-ANNO.

           PERFORM 004-IMPRIME-CLIENTE UNTIL WS-FIN-ARCHIVO = 1.

           IF WS-ENCONTRADO = 0 THEN
               DISPLAY " "
               DISPLAY "*** NO HAY CLIENTES PARA EL MES INTRODUCIDO ***"
               DISPLAY " "
               STOP "ENTER PARA CONTINUAR"
           END-IF.

           PERFORM 000-CERRAR-ARCHIVO-CLIENTES.
           DISPLAY " ".
           STOP "ENTER PARA CONTINUAR".

       004-LEE-SIG-CLIENTE.
           READ CLIENTES NEXT RECORD
               AT END 
                   MOVE 1 TO WS-FIN-ARCHIVO
               NOT AT END
                   CONTINUE
           END-READ.

       004-IMPRIME-CLIENTE.
           IF WS-MES = WS-MES AND WS-ANNO = WS-ANNO THEN
               DISPLAY "------------------------------------------"
               DISPLAY "-> RIF: " CLIENTE-RIF
               DISPLAY " - NOMBRE: " CLIENTE-NOMBRE
               DISPLAY " - RAZÓN SOCIAL: " CLIENTE-RAZON
               DISPLAY " - DIRECCIÓN: " CLIENTE-DIRECCION
               DISPLAY " - TELÉFONO: " CLIENTE-TELEFONO 
                       " / " CLIENTE-CELULAR
               DISPLAY " - CORREO ELECTRÓNICO: " CLIENTE-EMAIL
               MOVE 1 TO WS-ENCONTRADO
           END-IF.

           PERFORM 004-LEE-SIG-CLIENTE.
        
       002-LISTADO-FACTURAS.
           DISPLAY " "
           DISPLAY "********************************"
           DISPLAY "LISTADO MENSUAL DE FACTURAS: "
           DISPLAY "********************************"
           DISPLAY "INTRODUZCA EL MES (MM): " WITH NO ADVANCING.
           ACCEPT WS-MES.
           DISPLAY "INTRODUZCA EL AÑO (AAAA): " WITH NO ADVANCING.
           ACCEPT WS-ANNO.

           MOVE 0 TO WS-FIN-ARCHIVO.
           PERFORM 000-ABRIR-ARCHIVO-FACTURAS.
           PERFORM 004-LEE-SIG-FACTURA.

           MOVE 0 TO WS-ENCONTRADO.
           DISPLAY " "
           DISPLAY "FACTURAS PARA EL MES: " WS-MES "/" WS-ANNO.

           PERFORM 004-IMPRIME-FACTURA UNTIL WS-FIN-ARCHIVO = 1.

           IF WS-ENCONTRADO = 0 THEN
               DISPLAY " "
               DISPLAY "*** NO HAY FACTURAS PARA EL MES INTRODUCIDO ***"
               DISPLAY " "
               STOP "ENTER PARA CONTINUAR"
           END-IF.

           PERFORM 000-CERRAR-ARCHIVO-FACTURAS.
           DISPLAY " ".
           STOP "ENTER PARA CONTINUAR".

       004-LEE-SIG-FACTURA.
           READ FACTURAS NEXT RECORD
               AT END 
                   MOVE 1 TO WS-FIN-ARCHIVO
               NOT AT END
                   CONTINUE
           END-READ.

       004-IMPRIME-FACTURA.
           IF WS-MES = WS-MES AND WS-ANNO = WS-ANNO THEN
               DISPLAY "------------------------------------------"
               DISPLAY "-> RIF: " FACTURA-RIF
               DISPLAY " - NÚMERO DE FACTURA: " FACTURA-NUMERO
               DISPLAY " - RAZÓN SOCIAL: " FACTURA-RAZON
               DISPLAY " - PRODUCTO: " FACTURA-PRODUCTO
               DISPLAY " - PRECIO: " FACTURA-PRECIO
               DISPLAY " - DIRECCIÓN: " FACTURA-DIRECCION
               DISPLAY " - TELÉFONOS: " FACTURA-TELEFONOS
               DISPLAY " - IVA: " FACTURA-IVA
               DISPLAY " - TOTAL: " FACTURA-TOTAL
               MOVE 1 TO WS-ENCONTRADO
           END-IF.

           PERFORM 004-LEE-SIG-FACTURA.

       002-RESUMEN-VENTAS.
           DISPLAY " "
           DISPLAY "********************************"
           DISPLAY "RESUMEN MENSUAL DE VENTAS: "
           DISPLAY "********************************"
           DISPLAY "INTRODUZCA EL MES (MM): " WITH NO ADVANCING.
           ACCEPT WS-MES.
           DISPLAY "INTRODUZCA EL AÑO (AAAA): " WITH NO ADVANCING.
           ACCEPT WS-ANNO.

           MOVE 0 TO WS-FIN-ARCHIVO.
           PERFORM 000-ABRIR-ARCHIVO-VENTAS.
           PERFORM 004-LEE-SIG-VENTA.

           MOVE 0 TO WS-ENCONTRADO.
           DISPLAY " "
           DISPLAY "VENTAS PARA EL MES: " WS-MES "/" WS-ANNO.

           PERFORM 004-IMPRIME-VENTA UNTIL WS-FIN-ARCHIVO = 1.

           IF WS-ENCONTRADO = 0 THEN
               DISPLAY " "
               DISPLAY "*** NO HAY VENTAS PARA EL MES INTRODUCIDO ***"
               DISPLAY " "
               STOP "ENTER PARA CONTINUAR"
           END-IF.

           PERFORM 000-CERRAR-ARCHIVO-VENTAS.
           DISPLAY " ".
           STOP "ENTER PARA CONTINUAR".

       004-LEE-SIG-VENTA.
           READ VENTAS NEXT RECORD
               AT END 
                   MOVE 1 TO WS-FIN-ARCHIVO
               NOT AT END
                   CONTINUE
           END-READ.

       004-IMPRIME-VENTA.
           IF WS-MES = WS-MES AND WS-ANNO = WS-ANNO THEN
               DISPLAY "------------------------------------------"
               DISPLAY "-> CÉDULA: " VENTA-CEDULA
               DISPLAY " - NÚMERO DE FACTURA: " VENTA-FACTURA
               DISPLAY " - FECHA: " VENTA-FECHA
               DISPLAY " - CANTIDAD: " VENTA-CANTIDAD
               DISPLAY " - CATEGORÍA: " VENTA-CATEGORIA
               MOVE 1 TO WS-ENCONTRADO
           END-IF.

           PERFORM 004-LEE-SIG-VENTA.

       *> *************************************************************
       END PROGRAM RESTAURANTE.
       *> *************************************************************