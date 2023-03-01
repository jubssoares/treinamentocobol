      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. EXER0205.
       AUTHOR.     JULIANA SOARES.
      *================================================================*
      *    PROGRAMA....: EXER0205
      *    PROGRAMADOR.: JULIANA SOARES
      *    DATA........: 17/01/2023                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: GERAR ARQUIVO CSV, ONDE CADA REGISTRO CONTERA 
      *                  O VALOR TOTAL DE DEPOSITOS E A DATA DO DEPOSITO 
      *                  MAIS RECENTE, OBTIDOS EM UM ARQUIVO DE ENTRADA 
      *                  QUE CONTEM OS DEPOSITOS DOS CLIENTES POR CPF.
      *----------------------------------------------------------------*
      *    ARQUIVOS:                                                   *
      *       DDNAME                                 INCLUDE/BOOK      *
      *      ARQENT01                                  ENT02105
      *      ARQSAI01                                  SAI02105
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
.
           SELECT ARQSAI01 ASSIGN       TO UT-S-ARQSAI01
                      FILE STATUS      IS WRK-FS-ARQSAI01.
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================
      *                                                                *
      *----------------------------------------------------------------
       FILE                            SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------
      *    ARQUIVO DOS REGISTROS DE ENTRADA E SAIDA                    *
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    INPUT:     ARQUIVO DE ENTRADA                               *
      *               ORG. SEQUENCIAL   -   LRECL = 030                *
      *----------------------------------------------------------------*

       FD  ARQENT01
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS  0 RECORDS.
       01 FD-ARQENT01             PIC X(030).

      *---------------------------------------------------------------*
      *   OUTPUT:     ARQUIVO DE SAIDA                                *
      *               ORG. SEQUENCIAL   -   LRECL = 061               *
      *---------------------------------------------------------------*

       FD  ARQSAI01
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01 FD-ARQSAI01             PIC X(061).

      *
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *

      *----------------------------------------------------------------*
       77 FILLER                  PIC  X(050) VALUE
             'EXER0205 - INICIO DA AREA DE WORKING'.
      *----------------------------------------------------------------*
      *
       77 WRK-PROGRAMA            PIC X(008) VALUE 'EXER0205'.
       77 WRK-MASK-QTD-REG        PIC ZZ.ZZ9.
       77 WRK-ACU-LIDOS-ARQENT01  PIC 9(005) VALUE ZEROS.
       77 WRK-ACU-GRAVA-ARQSAI01  PIC 9(005) VALUE ZEROS.
       77 WRK-ACU-DEPOSITOS       PIC S9(017)V99 COMP-3 VALUE +0.
       
       01 WRK-DATA-LIDA-INV.
          03 WRK-ANO-LIDO         PIC 9(004) VALUE ZEROS.
          03 WRK-MES-LIDO         PIC 9(002) VALUE ZEROS.
          03 WRK-DIA-LIDO         PIC 9(002) VALUE ZEROS.
       
       01 WRK-DATA-RECENTE-INV.
          03 WRK-ANO-REC-INV      PIC 9(004) VALUE ZEROS.
          03 WRK-MES-REC-INV      PIC 9(002) VALUE ZEROS.
          03 WRK-DIA-REC-INV      PIC 9(002) VALUE ZEROS.

       01 WRK-DATA-RECENTE.
          03 WRK-DIA-RECENTE      PIC 9(002) VALUE ZEROS.
          03 WRK-MES-RECENTE      PIC 9(002) VALUE ZEROS.
          03 WRK-ANO-RECENTE      PIC 9(004) VALUE ZEROS.      

       01 WRK-CPF-ANT.
          03 WRK-COD-CPF-ANT      PIC 9(009) VALUE ZEROS.
          03 WRK-DIG-CPF-ANT      PIC 9(002) VALUE ZEROS.
       
       01 WRK-DATA-ANT-INV.
          03 WRK-ANO              PIC 9(004) VALUES ZEROS.
          03 WRK-MES              PIC 9(002) VALUES ZEROS.
          03 WRK-DIA              PIC 9(002) VALUES ZEROS.
       
       77 WRK-ARQUIVO             PIC X(008) VALUE SPACES.
          88 WRK-CN-ARQENT01      VALUE 'ENT02105'.
          88 WRK-CN-ARQSAI01      VALUE 'SAI02105'.

       77 WRK-COMANDO             PIC X(005) VALUE SPACES.
          88 WRK-CN-OPEN          VALUE 'OPEN '.
          88 WRK-CN-CLOSE         VALUE 'CLOSE'.
          88 WRK-CN-READ          VALUE 'READ '.
          88 WRK-CN-WRITE         VALUE 'WRITE'.
       
       01 WRK-CABEC.
          03 WRK-CABEC-ARQSAI01.
             05 FILLER           PIC X(036) VALUE 
             'CPF DO CLIENTE;DATA ULTIMO DEPOSITO;'.
             05 FILLER           PIC X(025) VALUE 
             'VALOR TOTAL DOS DEPOSITOS'.
      
      *----------------------------------------------------------------*
       01 FILLER                  PIC X(050) VALUE
             'AREA PARA TRATAMENTO DE FILE-STATUS'.
      *----------------------------------------------------------------*
      *
        01 WRK-AREA-FS.
          05 WRK-FS-ARQENT01      PIC X(002) VALUE SPACES.
             88 WRK-FS-ENT01-OK   VALUE '00'.
             88 WRK-FS-ENT01-FIM  VALUE '10'.
          05 WRK-FS-ARQSAI01      PIC X(002) VALUE SPACES.
             88 WRK-FS-SAI01-OK   VALUE '00'.
          05 WRK-FS-DISPLAY       PIC X(002) VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01 FILLER                  PIC  X(050) VALUE
             'AREA DOS BOOKS DOS ARQUIVOS DE ENTRADA E SAIDA'.
      *----------------------------------------------------------------*
      *
      **** AREA ARQUIVO DE ENTRADA E SAIDA

           COPY ENT02105.
           COPY SAI02105.
      
      *----------------------------------------------------------------*
       01 FILLER                  PIC  X(050) VALUE
             'EXER0105 - FIM DA AREA DE WORKING'.
      *----------------------------------------------------------------*
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA                                *
      *----------------------------------------------------------------*
       0000-PRINCIPAL SECTION.
      *----------------------------------------------------------------
      *
           PERFORM 1000-INICIALIZAR
      *
           PERFORM 3000-PROCESSAR UNTIL WRK-FS-ENT01-FIM
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
      *    ROTINA DE INICIALIZACAO DO PROGRAMA
      *----------------------------------------------------------------*
       1000-INICIALIZAR SECTION.
      *----------------------------------------------------------------*
      *
           SET WRK-CN-OPEN TO TRUE
           OPEN INPUT ARQENT01

           IF NOT WRK-FS-ENT01-OK 
              MOVE WRK-FS-ARQENT01 TO WRK-FS-DISPLAY 
              PERFORM 9000-ERROS-ARQUIVOS
           END-IF
      .
           OPEN OUTPUT ARQSAI01
           
           IF NOT WRK-FS-ENT01-OK 
              MOVE WRK-FS-ARQENT01 TO WRK-FS-DISPLAY 
              PERFORM 9000-ERROS-ARQUIVOS
           END-IF

           PERFORM 3800-LER-DEPOSITOS

           IF WRK-FS-ENT01-FIM
              DISPLAY '************************************************'
              DISPLAY  '*          ARQUIVO DE ENTRADA VAZIO           *'
              DISPLAY  '* PROGRAMA ' WRK-PROGRAMA 
                                     ' CANCELADO                      *'
              DISPLAY '************************************************'
              PERFORM 9900-FIM-PROGRAMA
           END-IF 

           SET WRK-CN-WRITE              TO TRUE
           SET WRK-CN-ARQSAI01           TO TRUE

           WRITE FD-ARQSAI01             FROM WRK-CABEC

           IF NOT WRK-FS-SAI01-OK 
              MOVE WRK-FS-ARQSAI01       TO WRK-FS-DISPLAY
              PERFORM 9000-ERROS-ARQUIVOS
           END-IF 

           MOVE ARQENT01-CPF             TO WRK-CPF-ANT
           .
      *
      *----------------------------------------------------------------*
       1000-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    CONTROLE DO PROCESSAMENTO ATÉ O FINAL DO ARQUIVO OU QUEBRA
      *    PARA GRAVAR A SAÍDA
      *----------------------------------------------------------------*
       3000-PROCESSAR SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 3100-TRATAMENTO-DEPOSITO
               UNTIL (ARQENT01-CPF NOT EQUAL WRK-CPF-ANT)
               OR    (WRK-FS-ENT01-FIM)
           
           PERFORM 3900-GRAVAR-SAIDA

           IF NOT WRK-FS-ENT01-FIM 
              MOVE ZEROS               TO WRK-ACU-DEPOSITOS 
              MOVE ARQENT01-CPF        TO WRK-CPF-ANT
              MOVE WRK-DATA-LIDA-INV   TO WRK-DATA-ANT-INV
                                          WRK-DATA-RECENTE-INV 
           END-IF

           .
      *
      *----------------------------------------------------------------*
       3000-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    ACUMULA DEPOSITOS E DATA MAIS RECENTE
      *----------------------------------------------------------------*
       3100-TRATAMENTO-DEPOSITO          SECTION.
      *----------------------------------------------------------------*
      *
           IF WRK-DATA-LIDA-INV GREATER WRK-DATA-RECENTE-INV 
              MOVE WRK-DATA-LIDA-INV TO WRK-DATA-RECENTE-INV
           END-IF

           COMPUTE WRK-ACU-DEPOSITOS = WRK-ACU-DEPOSITOS +
                                       ARQENT01-VAL-DEPOS 
           
           PERFORM 3800-LER-DEPOSITOS
           .
      *
      *----------------------------------------------------------------*
       3100-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
      *    ROTINA DE LEITURA DO ARQUIVO ARQENT01
      *----------------------------------------------------------------*
       3800-LER-DEPOSITOS SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                 ARQENT01-REGISTRO
           SET WRK-CN-READ            TO TRUE
           SET WRK-CN-ARQENT01        TO TRUE

           READ ARQENT01              INTO ARQENT01-REGISTRO 

           EVALUATE WRK-FS-ARQENT01 
               WHEN '00'
                    ADD 1 TO WRK-ACU-LIDOS-ARQENT01
                    MOVE ARQENT01-DAT-DEPOS (1:2) TO 
                         WRK-DATA-LIDA-INV (7:2)
                    MOVE ARQENT01-DAT-DEPOS (4:2) TO 
                         WRK-DATA-LIDA-INV (5:2) 
                    MOVE ARQENT01-DAT-DEPOS (7:4) TO 
                         WRK-DATA-LIDA-INV (1:4)
               WHEN '10'
                    CONTINUE 
               WHEN OTHER 
                    MOVE WRK-FS-ARQENT01 TO WRK-FS-DISPLAY 
                    PERFORM  9000-ERROS-ARQUIVOS
           END-EVALUATE 
           .
      *
      *----------------------------------------------------------------*
       3800-99-FIM.                     
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3900-GRAVAR-SAIDA SECTION.
      *----------------------------------------------------------------*
           
           MOVE WRK-DIA-REC-INV     TO WRK-DIA-RECENTE
           MOVE WRK-MES-REC-INV     TO WRK-MES-RECENTE
           MOVE WRK-ANO-REC-INV     TO WRK-ANO-RECENTE

           MOVE WRK-CPF-ANT         TO ARQSAI01-MASK-CPF
           MOVE WRK-DATA-RECENTE    TO ARQSAI01-MASK-DAT
           MOVE WRK-ACU-DEPOSITOS   TO ARQSAI01-MASK-VAL

           SET WRK-CN-WRITE         TO TRUE
           SET WRK-CN-ARQSAI01      TO TRUE
           
           WRITE FD-ARQSAI01        FROM ARQSAI01-REGISTRO

           IF NOT WRK-FS-SAI01-OK 
              MOVE WRK-FS-ARQSAI01  TO WRK-FS-DISPLAY
              PERFORM 9000-ERROS-ARQUIVOS
           END-IF 

           COMPUTE WRK-ACU-GRAVA-ARQSAI01 = WRK-ACU-GRAVA-ARQSAI01 + 1

           INITIALIZE ARQSAI01-REGISTRO 
           .
      
      *----------------------------------------------------------------*
       3900-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    ROTINA DE TRATAMENTO DE ERRO
      *----------------------------------------------------------------*
       9000-ERROS-ARQUIVOS SECTION.
      *----------------------------------------------------------------*

           DISPLAY '************************************************'
           DISPLAY '*        ERRO EM OPERACAO COM ARQUIVOS         *'
           DISPLAY '************************************************'
           DISPLAY '* COMANDO: ' WRK-COMANDO '                     *'
           DISPLAY '* ARQUIVO: ' WRK-ARQUIVO '                     *'
           DISPLAY '* FILE-STATUS:' WRK-FS-ARQENT01 '              *'
           DISPLAY '* PROGRAMA:' WRK-PROGRAMA '                    *'
           DISPLAY '************************************************'
           DISPLAY '*                  CANCELADO                   *'
           DISPLAY '************************************************'

           PERFORM 4000-FINALIZAR
           .
      *----------------------------------------------------------------*
       9000-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9900-FIM-PROGRAMA SECTION.
      *----------------------------------------------------------------*
           DISPLAY '************************************************'
           DISPLAY '*            PROGRAMA FINALIZADO               *'
           DISPLAY '************************************************'
           .
      *----------------------------------------------------------------*
       9900-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    ROTINA DE FINALIZACAO DO PROGRAMA
      *----------------------------------------------------------------*
       4000-FINALIZAR SECTION.
      *----------------------------------------------------------------*
           CLOSE ARQENT01.
           CLOSE ARQSAI01.

           SET WRK-CN-CLOSE TO TRUE.

           IF NOT WRK-FS-SAI01-OK AND WRK-FS-ENT01-FIM
              PERFORM 9000-ERROS-ARQUIVOS
           END-IF

           DISPLAY '**************************************************'
           DISPLAY '* QTDE DE REGISTROS LIDOS:' 
                    WRK-ACU-LIDOS-ARQENT01 '                         *'
           DISPLAY '* QTDE DE REGISTROS GRAVADOS:'
                    WRK-ACU-GRAVA-ARQSAI01 '                         *'
           DISPLAY '*                                                *'
           DISPLAY '* ' WRK-PROGRAMA ' FIM NORMAL                    *'
           DISPLAY '**************************************************'

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.
           EXIT.
      *----------------------------------------------------------------*