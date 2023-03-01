      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. EXER0305.
       AUTHOR.     JULIANA SOARES.
      *================================================================*
      *    PROGRAMA....: EXER0205                                      *
      *    PROGRAMADOR.: JULIANA SOARES                                *
      *    DATA........: 19/01/2023                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: RECEBER DUAS ENTRADAS E VERIFICAR O PAGAMENTO 
      *                  DO EMPRESTIMO, SE FOI FEITO NO PRAZO, FORA DO 
      *                  PRAZO OU SE NAO FOI FEITO. 
      *----------------------------------------------------------------*
      *    ARQUIVOS:                                                   *
      *       DDNAME                                 INCLUDE/BOOK      *
      *      ARQENT01                                  ENT03105        *
      *      ARQENT02                                  ENT03205        *
      *      ARQSAI01                                  SAI03105        *
      *      ARQSAI02                                  SAI03205        *
      *      ARQSAI03                                  SAI03205        *
      *      ARQSAI04                                  SAI03205        *
      *----------------------------------------------------------------*
      *    ROTINAS.....:                                               *
      *                                                                *
      *================================================================*
      *                                                                *
      *================================================================*
       ENVIRONMENT                     DIVISION.                        
      *================================================================*
      *                                                                *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.                         
      *----------------------------------------------------------------*
      *                                                                 
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT               IS COMMA.                        
      *                                                                 
      *----------------------------------------------------------------* 
       INPUT-OUTPUT                    SECTION.                         
      *----------------------------------------------------------------*
      *                                                                 
       FILE-CONTROL.                                                    
      *                                                                 
           SELECT ARQENT01 ASSIGN      TO UT-S-ARQENT01                 
                       FILE STATUS      IS WRK-FS-ARQENT01.              
           SELECT ARQENT02 ASSIGN      TO UT-S-ARQENT02                 
                       FILE STATUS      IS WRK-FS-ARQENT02.              
 .                                                                       
           SELECT ARQSAI01 ASSIGN       TO UT-S-ARQSAI01         
                       FILE STATUS      IS WRK-FS-ARQSAI01. 
           SELECT ARQSAI02 ASSIGN       TO UT-S-ARQSAI02         
                       FILE STATUS      IS WRK-FS-ARQSAI02.
           SELECT ARQSAI03 ASSIGN       TO UT-S-ARQSAI03         
                       FILE STATUS      IS WRK-FS-ARQSAI03.
           SELECT ARQSAI04 ASSIGN       TO UT-S-ARQSAI04         
                       FILE STATUS      IS WRK-FS-ARQSAI04.
      
      *                                                                 
      *================================================================*
       DATA                            DIVISION.                        
      *================================================================*
      *                                                                *
      *----------------------------------------------------------------*
       FILE                            SECTION.                         
      *----------------------------------------------------------------*
      *                                                                 
      *---------------------------------------------------------------- 
      *    ARQUIVO DOS REGISTROS DE ENTRADA E SAIDA                    *
      *----------------------------------------------------------------*
      *                                                                 
      *----------------------------------------------------------------*
      *    INPUT:     ARQUIVO DE ENTRADA                               *
      *               ORG. SEQUENCIAL   -   LRECL = 056                *
      *----------------------------------------------------------------*
                                                                         
       FD  ARQENT01                                                     
            RECORDING MODE IS F                                          
            LABEL RECORD   IS STANDARD                                   
            BLOCK CONTAINS  0 RECORDS.                                   
       01 FD-ARQENT01             PIC X(056).
      *                                                                 
      *----------------------------------------------------------------*
      *    INPUT:     ARQUIVO DE ENTRADA                               *
      *               ORG. SEQUENCIAL   -   LRECL = 016                *
      *----------------------------------------------------------------*

       FD  ARQENT02                                                     
            RECORDING MODE IS F                                          
            LABEL RECORD   IS STANDARD                                   
            BLOCK CONTAINS  0 RECORDS.                                   
       01 FD-ARQENT01             PIC X(016).
      *
      * 
      *---------------------------------------------------------------*
      *   OUTPUT:     ARQUIVOS DE SAIDA                               *
      *               ORG. SEQUENCIAL   -   LRECL = 016               *
      *---------------------------------------------------------------*

       FD  ARQSAI01
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01 FD-ARQSAI01             PIC X(016).
      * 
      *---------------------------------------------------------------*
      *   OUTPUT:     ARQUIVOS DE SAIDA                               *
      *               ORG. SEQUENCIAL   -   LRECL = 006               *
      *---------------------------------------------------------------*

       FD  ARQSAI02
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01 FD-ARQSAI02             PIC X(006).
      * 
      *---------------------------------------------------------------*
      *   OUTPUT:     ARQUIVOS DE SAIDA                               *
      *               ORG. SEQUENCIAL   -   LRECL = 006               *
      *---------------------------------------------------------------*

       FD  ARQSAI03
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01 FD-ARQSAI03             PIC X(006).
      * 
      *---------------------------------------------------------------*
      *   OUTPUT:     ARQUIVOS DE SAIDA                               *
      *               ORG. SEQUENCIAL   -   LRECL = 006               *
      *---------------------------------------------------------------*

       FD  ARQSAI04
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01 FD-ARQSAI04             PIC X(006).

      *
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.                         
      *----------------------------------------------------------------*
                                                                         
      *----------------------------------------------------------------*
        77 FILLER                  PIC  X(050) VALUE                     
              'EXER0305 - INICIO DA AREA DE WORKING'.                 
      *----------------------------------------------------------------*
      *                                                                 
        77 WRK-PROGRAMA            PIC  X(008)         VALUE 'EXER0305'.         
        77 WRK-MASK-QTDREG         PIC  ZZ.ZZ9.                          
        77 ACU-LIDOS-ARQENT01      PIC  9(005)         VALUE ZEROS.
        77 ACU-LIDOS-ARQENT02      PIC  9(005)         VALUE ZEROS.                           
        77 ACU-GRAVA-ARQSAI01      PIC  9(005)         VALUE ZEROS. 
        77 ACU-GRAVA-ARQSAI02      PIC  9(005)         VALUE ZEROS.
        77 ACU-GRAVA-ARQSAI03      PIC  9(005)         VALUE ZEROS.
        77 ACU-GRAVA-ARQSAI04      PIC  9(005)         VALUE ZEROS.
        
        77 WRK-ARQUIVO             PIC  X(008) VALUE SPACES.             
           88 WRK-CN-ARQENT01      VALUE 'ARQENT01'.    
           88 WRK-CN-ARQENT02      VALUE 'ARQENT02'.                                      
           88 WRK-CN-ARQSAI01      VALUE 'ARQSAI01'.                     
           88 WRK-CN-ARQSAI02      VALUE 'ARQSAI02'.
           88 WRK-CN-ARQSAI03      VALUE 'ARQSAI03'.
           88 WRK-CN-ARQSAI04      VALUE 'ARQSAI04'.
        
        77 WRK-COMANDO             PIC  X(005) VALUE SPACES.             
           88 WRK-CN-OPEN          VALUE 'OPEN '.                        
           88 WRK-CN-CLOSE         VALUE 'CLOSE'.                        
           88 WRK-CN-READ          VALUE 'READ '.                        
           88 WRK-CN-WRITE         VALUE 'WRITE'. 
        
        01 WRK-CHAVE-CADASTRO.
           03 WRK-COD-AG-C         PIC 9(03) VALUE 0.            
           03 WRK-NUM-CNT-C        PIC 9(03) VALUE 0. 
        
        01 WRK-CHAVE-MOVIMENTO.
           03 WRK-COD-AG-M          PIC 9(03) COMP-3 VALUE +0.
           03 WRK-NUM-CNT-M         PIC 9(03) COMP-3 VALUE +0.
        
      *----------------------------------------------------------------*
       01  FILLER                   PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM CALE2000 *'.
      *----------------------------------------------------------------*

        77  WRK-CALE2000            PIC  X(008) VALUE 'CALE2000'.

      *     
      *----------------------------------------------------------------* 
        01 FILLER                  PIC  X(050) VALUE                     
              'AREA PARA TRATAMENTO DE FILE-STATUS'.                     
      *----------------------------------------------------------------*
      *                                                                 
        01 WRK-AREA-FS.        

           05 WRK-FS-ARQENT01      PIC  X(002) VALUE SPACES.             
              88 WRK-FS-ENT01-OK   VALUE '00'.                           
              88 WRK-FS-ENT01-FIM  VALUE '10'.                           
           05 WRK-FS-ARQENT02      PIC  X(002) VALUE SPACES.             
              88 WRK-FS-ENT02-OK   VALUE '00'.                           
              88 WRK-FS-ENT02-FIM  VALUE '10'.                           

           05 WRK-FS-ARQSAI01      PIC  X(002) VALUE SPACES.             
              88 WRK-FS-SAI01-OK   VALUE '00'.                           
           05 WRK-FS-ARQSAI02      PIC  X(002) VALUE SPACES.             
              88 WRK-FS-SAI02-OK   VALUE '00'.
           05 WRK-FS-ARQSAI03      PIC  X(002) VALUE SPACES.             
              88 WRK-FS-SAI03-OK   VALUE '00'.
           05 WRK-FS-ARQSAI04      PIC  X(002) VALUE SPACES.             
              88 WRK-FS-SAI04-OK   VALUE '00'.
           
           05 WRK-FS-DISPLAY       PIC  X(002) VALUE SPACES. 
      
      *                                                                 
      *----------------------------------------------------------------*
        01 FILLER                  PIC  X(050) VALUE                     
              'AREA DOS BOOKS DOS ARQUIVOS DE ENTRADA E SAIDA'.      
      *----------------------------------------------------------------*
      *  
       COPY ENT03105.
       COPY ENT03205.
       COPY SAI03105.
       COPY SAI03205.
       COPY 'I#CALE01'.
      
      *----------------------------------------------------------------*
        01 FILLER                  PIC  X(050) VALUE                     
              'EXER0305 - FIM DA AREA DE WORKING'.                       
      *----------------------------------------------------------------*
      *================================================================*
       PROCEDURE                       DIVISION.                        
      *================================================================*
      *                                                                 
      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA                                *
      *----------------------------------------------------------------*
       0000-PRINCIPAL SECTION.                                          
      *----------------------------------------------------------------* 
      *                                                                 
           PERFORM 1000-INICIALIZAR                            
      *                                                                 
           PERFORM 3000-PROCESSAR                                       
                UNTIL (WRK-FS-ENT01-FIM)
                AND   (WRK-FS-ENT02-FIM)                                   
      *                                                                 
           PERFORM 4000-FINALIZAR                                       
            .                                                            
      *                                                                 
      *----------------------------------------------------------------*
        0000-99-FIM.                                                     
            EXIT.                                                        
      *----------------------------------------------------------------*
      *                                                                 
      *----------------------------------------------------------------* 
      *    ROTINA DE INICIALIZAÇÃO DO PROGRAMA                         *
      *----------------------------------------------------------------* 
       1000-INICIALIZAR                SECTION.                         
      *----------------------------------------------------------------*
      *                                                                 
            SET WRK-CN-OPEN             TO TRUE                          
            OPEN INPUT ARQENT01         SET WRK-CN-ARQENT01 TO TRUE                                    
      *                                                                 
            IF NOT WRK-FS-ENT01-OK                                           
               MOVE WRK-FS-ARQENT01     TO WRK-FS-DISPLAY                
               PERFORM 9100-ERROS-ARQUIVOS                               
            END-IF  

            OPEN INPUT ARQENT02
            SET WRK-CN-ARQENT02         TO TRUE                          
                                          
      *                                                                 
            IF NOT WRK-FS-ENT02-OK                                           
               MOVE WRK-FS-ARQENT02     TO WRK-FS-DISPLAY                
               PERFORM 9100-ERROS-ARQUIVOS                               
            END-IF

            OPEN OUTPUT ARQSAI01
            SET WRK-CN-ARQSAI01         TO TRUE                                                                   
      *                                                                 
            IF NOT WRK-FS-SAI01-OK                                       
               MOVE WRK-FS-ARQSAI01     TO WRK-FS-DISPLAY                
               PERFORM 9100-ERROS-ARQUIVOS                               
            END-IF                                                       

            OPEN OUTPUT ARQSAI02
            SET WRK-CN-ARQSAI02         TO TRUE                                                                 
      *                                                                 
            IF NOT WRK-FS-SAI02-OK                                       
               MOVE WRK-FS-ARQSAI02     TO WRK-FS-DISPLAY                
               PERFORM 9100-ERROS-ARQUIVOS                               
            END-IF     

            OPEN OUTPUT ARQSAI03
            SET WRK-CN-ARQSAI03         TO TRUE                                                                 
      *                                                                 
            IF NOT WRK-FS-SAI03-OK                                       
               MOVE WRK-FS-ARQSAI03     TO WRK-FS-DISPLAY                
               PERFORM 9100-ERROS-ARQUIVOS                               
            END-IF  

            OPEN OUTPUT ARQSAI04
            SET WRK-CN-ARQSAI04         TO TRUE                                                                 
      *                                                                 
            IF NOT WRK-FS-SAI04-OK                                       
               MOVE WRK-FS-ARQSAI04     TO WRK-FS-DISPLAY                
               PERFORM 9100-ERROS-ARQUIVOS                               
            END-IF       

            PERFORM 3800-LER-CADASTRO                                    
                                                                         
            IF WRK-FS-ENT01-FIM                                          
              DISPLAY '************************************************'
              DISPLAY '*          ARQUIVO DE ENTRADA 01 VAZIO         *'
              DISPLAY '* PROGRAMA ' WRK-PROGRAMA                        
                                         ' CANCELADO                  *'
              DISPLAY '************************************************'
              PERFORM 9900-FIM-PROGRAMA                                 
            END-IF                                                       
                     
            PERFORM 3850-LER-MOVIMENTO                                    
                                                                         
            IF WRK-FS-ENT02-FIM                                          
              DISPLAY '************************************************'
              DISPLAY '*          ARQUIVO DE ENTRADA 02 VAZIO         *'
              DISPLAY '* PROGRAMA ' WRK-PROGRAMA                        
                                         ' CANCELADO                  *'
              DISPLAY '************************************************'
              PERFORM 9900-FIM-PROGRAMA                                 
            END-IF

            .                                                                                 
      *                                                                 
      *----------------------------------------------------------------*
        1000-99-FIM.                                                     
            EXIT.                                                        
      *----------------------------------------------------------------*
                                                                         
      *----------------------------------------------------------------*
      *    BALANCE LINE
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.                                          
      *----------------------------------------------------------------*
      *

           EVALUATE TRUE

           WHEN WRK-CHAVE-CADASTRO EQUAL WRK-CHAVE-MOVIMENTO
      * CALL 
              PERFORM 3500-CALCULA-QTDE-DIAS
              PERFORM 3800-LER-CADASTRO
              PERFORM 3850-LER-MOVIMENTO
           
           WHEN WRK-CHAVE-CADASTRO LESS WRK-CHAVE-MOVIMENTO
              PERFORM 3300-MONTA-SAIDA3
              PERFORM 3930-GRAVA-SAIDA3
              PERFORM 3800-LER-CADASTRO
           
           WHEN WRK-CHAVE-CADASTRO GREATER WRK-CHAVE-MOVIMENTO
              PERFORM 3400-MONTA-SAIDA4
              PERFORM 3940-GRAVA-SAIDA4
              PERFORM 3850-LER-MOVIMENTO    

       END-EVALUATE

       .                                                            
      *                                                                 
      *----------------------------------------------------------------*
        3000-99-FIM.                                                     
            EXIT.                                                        
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA OBTER QTDE DE DIAS CORRIDOS ENTRE DUAS DATAS    *
      *----------------------------------------------------------------*
       3500-CALCULA-QTDE-DIAS          SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE CALE01-REGISTRO.

           MOVE 'CALE0001'            TO CALE01-ID-BLOCO
           MOVE LENGTH                OF CALE01-REGISTRO
                                      TO CALE01-TAM-BLOCO.
           MOVE 'F3'                  TO CALE01-FUNCAO.
           MOVE 'SF3004'              TO CALE01-SUB-FUNCAO.

      * MONTA DATA INICIAL PARA CÁLCULO DA DIFERENCA ENTRE DATAS
      * 007: PARAMETRO INDICA FORMATO DD.MM.AAAA
   
           MOVE  007                  TO CALE01-FORMATO-ARGUMENTO-INI
           MOVE ARQENT01-DAT-EMPRE    TO CALE01-VLR-ARGUMENTO-INI.

      * MONTA DATA FINAL PARA CÁLCULO DA DIFERENCA ENTRE DATAS
      * 003: PARAMETRO INDICA FORMATO AAAAMMDD

           MOVE  003                  TO CALE01-FORMATO-ARGUMENTO-FINAL.
           MOVE ARQENT02-DAT-PAGTO    TO CALE01-VLR-ARGUMENTO-FINAL.
           
      * I: PARAMETRO INDICA QUE DATAS INICIO E FIM DEVEM SER INCLUIDAS
      *    NO CÁLCULO

           MOVE 'I'                   TO CALE01-TP-INCL-ARGUMENTO-INI
                                         CALE01-TP-INCL-ARGUMENTO-FINAL

           MOVE ZEROS                 TO CALE01-COD-IDIOMA
           MOVE ZEROS                 TO CALE01-COD-LOCALIDADE

           CALL WRK-CALE2000          USING CALE01-REGISTRO.

           EVALUATE CALE01-COD-RETORNO
           
           WHEN ZEROS
      * RETORNO OK DA ROTINA
               IF CALE01-QTDE-DIAS-PERIODO GREATER 30
                  PERFORM 3200-MONTA-SAIDA2
                  PERFORM 3940-GRAVA-SAIDA2
               ELSE
                  PERFORM 3100-MONTA-SAIDA1
                  PERFORM 3930-GRAVA-SAIDA1
               END-IF
           WHEN OTHER
      * RETORNO NAO OK DA ROTINA
                  DISPLAY '********************************************'
                  DISPLAY '*        ERRO NA ROTINA CALE2000           *'
                  DISPLAY '********************************************'
                  PERFORM 9100-ERROS-ARQUIVOS

           END-EVALUATE
           .
      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * PAGAMENTO NO PRAZO                                             *
      *----------------------------------------------------------------*
       3100-MONTA-SAIDA1 SECTION.
      *----------------------------------------------------------------*
      *
  	          MOVE ARQENT02-COD-AGENCIA     TO ARQSAI01-COD-AGENCIA
	          MOVE ARQENT02-NUM-CONTA       TO ARQSAI01-NUM-CONTA
	          MOVE ARQENT02-DAT-PAGTO       TO ARQSAI01-DAT-PAGTO
           .
      *                                                                *
      *----------------------------------------------------------------*
       3100-99-FIM.
           EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * PAGAMENTO APOS O PRAZO                                         *
      *----------------------------------------------------------------*
       3200-MONTA-SAIDA2 SECTION.
      *----------------------------------------------------------------*
      *
             MOVE ARQENT02-COD-AGENCIA     TO ARQSAI01-COD-AGENCIA
	          MOVE ARQENT02-NUM-CONTA       TO ARQSAI01-NUM-CONTA
	          MOVE ARQENT02-DAT-PAGTO       TO ARQSAI01-DAT-PAGTO
           .
      *----------------------------------------------------------------*
       3200-99-FIM.
           EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * PAGAMENTO NAO REALIZADO                                        *
      *----------------------------------------------------------------*
       3300-MONTA-SAIDA3 SECTION.
      *----------------------------------------------------------------*
      *
             MOVE ARQENT01-COD-AGENCIA     TO ARQSAI02-COD-AGENCIA
	          MOVE ARQENT01-NUM-CONTA       TO ARQSAI02-NUM-CONTA  
           .       
      *                                                                *
      *----------------------------------------------------------------*
       3300-99-FIM.
           EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * CLIENTE NAO CADASTRADO                                         *
      *----------------------------------------------------------------*
       3400-MONTA-SAIDA4 SECTION.
      *----------------------------------------------------------------*
      *
             MOVE ARQENT02-COD-AGENCIA     TO ARQSAI02-COD-AGENCIA
	          MOVE ARQENT02-NUM-CONTA       TO ARQSAI02-NUM-CONTA  
           .       
      *                                                                *
      *----------------------------------------------------------------*
       3400-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
      
      *----------------------------------------------------------------*
      *    ROTINA DE LEITURA DO ARQUIVO CADASTRO                        
      *----------------------------------------------------------------*
       3800-LER-CADASTRO               SECTION.                         
      *----------------------------------------------------------------*
      * 
            INITIALIZE                  ARQENT01-REGISTRO                
            SET WRK-CN-READ             TO TRUE                          
            SET WRK-CN-ARQENT01         TO TRUE

            READ ARQENT01                   INTO ARQENT01-REGISTRO
        
            EVALUATE WRK-FS-ARQENT01                                     
               WHEN '00'                                                
                     ADD 1 TO ACU-LIDOS-ARQENT01
                     MOVE ARQENT01-COD-AGENCIA TO WRK-COD-AG-C 
                     MOVE ARQENT01-NUM-CONTA   TO WRK-NUM-CNT-C          
               WHEN '10'                                                
                     MOVE LOW-VALUES TO ARQENT01-COD-AGENCIA
               WHEN OTHER                                               
                    MOVE WRK-FS-ARQENT01 TO WRK-FS-DISPLAY              
                    PERFORM 9100-ERROS-ARQUIVOS                         
            END-EVALUATE
        .                                              
                                                                         
      *                                                                 
      *----------------------------------------------------------------*
        3800-99-FIM.                                                     
            EXIT.                                                        
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE LEITURA DO ARQUIVO MOVIMENTO                        
      *----------------------------------------------------------------*
       3850-LER-MOVIMENTO               SECTION.                         
      *----------------------------------------------------------------*
      *                                                                 
                                                                 
            INITIALIZE                  ARQENT02-REGISTRO                
            SET WRK-CN-READ             TO TRUE                          
            SET WRK-CN-ARQENT02         TO TRUE                          
                                                                         
            READ ARQENT02               INTO ARQENT02-REGISTRO
      *                                                                 
            EVALUATE WRK-FS-ARQENT02                                     
                WHEN '00'                                                
                      ADD 1        TO ACU-LIDOS-ARQENT02
                      MOVE ARQENT02-COD-AGENCIA TO WRK-COD-AG-M
                      MOVE ARQENT02-NUM-CONTA   TO WRK-NUM-CNT-M     
                WHEN '10'                                                
                      MOVE HIGH-VALUES  TO ARQENT02-COD-AGENCIA          
                WHEN OTHER                                               
                     MOVE WRK-FS-ARQENT02 TO WRK-FS-DISPLAY              
                     PERFORM 9100-ERROS-ARQUIVOS                         
            END-EVALUATE                                                 
        .                                                            
                                                                         
      *                                                                 
      *----------------------------------------------------------------*
        3850-99-FIM.                                                     
            EXIT.                                                        
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       3910-GRAVA-SAIDA1                SECTION.                         
      *----------------------------------------------------------------*
            SET WRK-CN-WRITE            TO TRUE                          
            SET WRK-CN-ARQSAI01         TO TRUE                          
                                                                         
            WRITE FD-ARQSAI01           FROM ARQSAI01-REGISTRO           
                                                                         
            IF NOT WRK-FS-SAI01-OK                                       
               MOVE WRK-FS-ARQSAI01     TO WRK-FS-DISPLAY                
               PERFORM 9100-ERROS-ARQUIVOS                               
            END-IF                                                       
                                                                         
            COMPUTE ACU-GRAVA-ARQSAI01 = ACU-GRAVA-ARQSAI01 + 1          
                                                                         
            INITIALIZE                  ARQSAI01-REGISTRO                         
            .                                                            
                                                                         
      *----------------------------------------------------------------*
       3910-99-FIM.                                                     
            EXIT.                                                        
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       3920-GRAVA-SAIDA2                SECTION.                         
      *----------------------------------------------------------------*

           SET WRK-CN-WRITE TO TRUE
           SET WRK-CN-ARQSAI02 TO TRUE

           WRITE FD-ARQSAI02 FROM ARQSAI01-REGISTRO

           IF NOT WRK-FS-SAI02-OK
              MOVE WRK-FS-ARQSAI02 TO WRK-FS-DISPLAY
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF

           COMPUTE ACU-GRAVA-ARQSAI02 = ACU-GRAVA-ARQSAI02 + 1

           INITIALIZE ARQSAI02-REGISTRO
           .
      *----------------------------------------------------------------*
       3920-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
      * 
      *----------------------------------------------------------------*
       3930-GRAVA-SAIDA3 SECTION.
      *----------------------------------------------------------------*

           SET WRK-CN-WRITE TO TRUE
           SET WRK-CN-ARQSAI03 TO TRUE

           WRITE FD-ARQSAI03 FROM ARQSAI02-REGISTRO

           IF NOT WRK-FS-SAI03-OK
              MOVE WRK-FS-ARQSAI03 TO WRK-FS-DISPLAY
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF

           COMPUTE ACU-GRAVA-ARQSAI03 = ACU-GRAVA-ARQSAI03 + 1

           INITIALIZE ARQSAI02-REGISTRO
           .

      *----------------------------------------------------------------*
       3930-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       3940-GRAVA-SAIDA4 SECTION.
      *----------------------------------------------------------------*

           SET WRK-CN-WRITE TO TRUE
           SET WRK-CN-ARQSAI04 TO TRUE

           WRITE FD-ARQSAI04 FROM ARQSAI02-REGISTRO

           IF NOT WRK-FS-SAI04-OK
              MOVE WRK-FS-ARQSAI04 TO WRK-FS-DISPLAY
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF

           COMPUTE ACU-GRAVA-ARQSAI04 = ACU-GRAVA-ARQSAI04 + 1

           INITIALIZE ARQSAI02-REGISTRO
           .

      *----------------------------------------------------------------*
       3940-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
      *
      *---------------------------------------------------------------- 
        4000-FINALIZAR             SECTION.                              
      *----------------------------------------------------------------*
                                                                         
            SET WRK-CN-CLOSE            TO TRUE                          
                                                                         
            SET WRK-CN-ARQENT01         TO TRUE                          
                                                                         
            CLOSE ARQENT01                                               
            IF NOT WRK-FS-ENT01-OK                                       
               MOVE WRK-FS-ARQENT01     TO WRK-FS-DISPLAY                
               PERFORM 9100-ERROS-ARQUIVOS                               
            END-IF                                                       
                                                                         
            SET WRK-CN-ARQENT02         TO TRUE                          
                                                                         
            CLOSE ARQENT02                                               
            IF NOT WRK-FS-ENT02-OK                                       
               MOVE WRK-FS-ARQENT02     TO WRK-FS-DISPLAY                
               PERFORM 9100-ERROS-ARQUIVOS                               
            END-IF                                                       
                                                                         
            SET WRK-CN-ARQSAI01         TO TRUE                          
                                                                         
            CLOSE ARQSAI01                                               
            IF NOT WRK-FS-SAI01-OK                                       
               MOVE WRK-FS-ARQSAI01     TO WRK-FS-DISPLAY                
               PERFORM 9100-ERROS-ARQUIVOS                               
            END-IF                                                       

            SET WRK-CN-ARQSAI02         TO TRUE                          
                                                                         
            CLOSE ARQSAI02                                               
            IF NOT WRK-FS-SAI02-OK                                       
               MOVE WRK-FS-ARQSAI02     TO WRK-FS-DISPLAY                
               PERFORM 9100-ERROS-ARQUIVOS                               
            END-IF
            
            MOVE ACU-LIDOS-ARQENT01     TO WRK-MASK-QTDREG               
            DISPLAY '**********************************************'     
            DISPLAY '* QTDE. LIDOS CADASTRO       : ' WRK-MASK-QTDREG    
                                                         '        *'     
            MOVE ACU-LIDOS-ARQENT02     TO WRK-MASK-QTDREG               
            DISPLAY '**********************************************'     
            DISPLAY '* QTDE. LIDOS MOVIMENTO      : ' WRK-MASK-QTDREG    
                                                         '        *'     

            MOVE ACU-GRAVA-ARQSAI01     TO WRK-MASK-QTDREG               
            DISPLAY '* QTDE. GRAVADOS VACINADOS   : ' WRK-MASK-QTDREG    
                                                         '        *'     
            MOVE ACU-GRAVA-ARQSAI02     TO WRK-MASK-QTDREG               
            DISPLAY '* QTD. GRAVADOS NAO VACINADOS: ' WRK-MASK-QTDREG    
                                                         '        *'     
            MOVE ACU-GRAVA-ARQSAI03     TO WRK-MASK-QTDREG               
            DISPLAY '* QT.GRAVADOS NAO CADASTRADOS: ' WRK-MASK-QTDREG    
                                                         '        *'     

            DISPLAY '* ' WRK-PROGRAMA                                    
                              ' FIM NORMAL                        *'     
            DISPLAY '**********************************************'     
                                                                         
            PERFORM 9900-FIM-PROGRAMA                                    
            .                                                            
                                                                         
      *----------------------------------------------------------------*
        4000-99-FIM.                       
            EXIT.                                                        
      *----------------------------------------------------------------*
      *
      *---------------------------------------------------------------- 
        9100-ERROS-ARQUIVOS        SECTION.                              
      *----------------------------------------------------------------*
                                                                         
            DISPLAY '************************************************'   
            DISPLAY '*       ERRO EM OPERAÇÃO COM ARQUIVOS          *'   
            DISPLAY '* COMANDO    : ' WRK-COMANDO                        
                                        '                           *'   
            DISPLAY '* ARQUIVO    : ' WRK-ARQUIVO                        
                                           '                        *'   
            DISPLAY '* FILE-STATUS: ' WRK-FS-DISPLAY                     
                                      '                             *'   
            DISPLAY '* PROGRAMA ' WRK-PROGRAMA                           
                                       ' CANCELADO                  *'   
            DISPLAY '************************************************'   
                                                                         
            PERFORM 9900-FIM-PROGRAMA                                    
            .                                                            
                                                                         
      *----------------------------------------------------------------*
        9100-99-FIM.                                                     
            EXIT.                                                        
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       9900-FIM-PROGRAMA SECTION.
      *----------------------------------------------------------------*
           DISPLAY '************************************************'
           DISPLAY '*            PROGRAMA FINALIZADO               *'
           DISPLAY '************************************************'

           STOP RUN
           .
      *----------------------------------------------------------------*
       9900-99-FIM.
           EXIT.
      *----------------------------------------------------------------*