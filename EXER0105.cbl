      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. EXER0105.
       AUTHOR.     JULIANA SOARES.
      *================================================================*
      *    PROGRAMA....: EXER0105
      *    PROGRAMADOR.: JULIANA SOARES
      *    DATA........: 11/01/2023                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR ARQUIVO CSV, EXTRAINDO DO CADASTRO    *
      *                    DE FUNCIONARIOS AQUELES CUJA DATA DE        *
      *                    CONTROLE INTERNO NAO ESTEJA EXPIRADA.       *
      *----------------------------------------------------------------*
      *    ARQUIVOS:                                                   *
      *       DDNAME                                 INCLUDE/BOOK      *
      *      ARQENT01                                  ENT01105
      *      ARQSAI01                                  SAI01105
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
      *----------------------------------------------------------------
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
      *               ORG. SEQUENCIAL   -   LRECL = 168                *
      *----------------------------------------------------------------*

       FD  ARQENT01
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS  0 RECORDS.
       01 FD-ARQENT01             PIC X(168).

      *---------------------------------------------------------------*
      *   OUTPUT:     ARQUIVO DE SAIDA                                *
      *               ORG. SEQUENCIAL   -   LRECL = 92                *
      *---------------------------------------------------------------*

       FD  ARQSAI01
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01 FD-ARQSAI01             PIC X(92).

      *
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *

      *----------------------------------------------------------------*
       77 FILLER                  PIC  X(050) VALUE
             'EXER0105 - INICIO DA AREA DE WORKING'.
      *----------------------------------------------------------------*
      *
       77 WRK-PROGRAMA            PIC  X(008) VALUE 'EXER0105'.
       77 ACU-LIDOS-ARQENT01      PIC  9(005) VALUE ZEROS.
       77 ACU-GRAVA-ARQSAI01      PIC  9(005) VALUE ZEROS.
       77 WRK-DATA-ENTR-INV       PIC  9(008) VALUE ZEROS.
       77 WRK-DATA-CORRENTE       PIC  9(008) VALUE ZEROS. 
      *
       01 WRK-DATA-CONTR-INV.
          05 WRK-ANO              PIC  9(004) VALUE ZEROS.
          05 WRK-MES              PIC  9(002) VALUE ZEROS.
          05 WRK-DIA              PIC  9(002) VALUE ZEROS.
       
       01 WRK-CABEC.
          05 WRK-CABEC-ARQSAI01   PIC  X(048) VALUE
                'NOME DO FUNCIONARIO;ESCRITORIO;DATA DE EXPIRACAO'.             
           
      *----------------------------------------------------------------
       01 FILLER                  PIC  X(050) VALUE
             'AREA PARA TRATAMENTO DE FILE-STATUS'.
      *----------------------------------------------------------------*
      *
       01 WRK-AREA-FS.
          05 WRK-FS-ARQENT01      PIC  X(002) VALUE SPACES.
             88 WRK-FS-ENT01-OK               VALUE '00'.
             88 WRK-FS-ENT01-FIM              VALUE '10'.

      *
       01 WRK-FS-ARQSAI01         PIC  X(002) VALUE SPACES.
          88 WRK-FS-SAI01-OK                  VALUE '00'.
      *
      *----------------------------------------------------------------*
       01 FILLER                  PIC  X(050) VALUE
             'AREA DOS BOOKS DOS ARQUIVOS DE ENTRADA E SAIDA'.
      *----------------------------------------------------------------*
      *
      **** AREA ARQUIVO DE ENTRADA E SAIDA

           COPY ENT01105.
           COPY SAI01105.

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
      *----------------------------------------------------------------
      *    ROTINA DE INICIALIZACAO DO PROGRAMA
      *----------------------------------------------------------------*
       1000-INICIALIZAR SECTION.
      *----------------------------------------------------------------*
      *
           OPEN INPUT ARQENT01
                OUTPUT ARQSAI01
      *
           IF WRK-FS-ENT01-OK AND WRK-FS-ARQSAI01
              MOVE FUNCTION CURRENT-DATE(1:8) TO WRK-DATA-CORRENTE  
           ELSE 
              DISPLAY '************************************************'
              DISPLAY '*       ERRO EM OPERAÇÃO COM ARQUIVOS          *'
              DISPLAY '* COMANDO: OPEN                                *'
              DISPLAY '* FILE-STATUS ENT:' WRK-FS-ARQENT01 '          *'
              DISPLAY '* FILE-STATUS SAI:' WRK-FS-ARQSAI01 '          *'
              DISPLAY '* PROGRAMA EXER0105 CANCELADO                  *'
              DISPLAY '************************************************'
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF 
           
           PERFORM 3800-LER-CADASTRO
           
           IF WRK-FS-ENT01-FIM
              DISPLAY '************************************************'
              DISPLAY '*       ERRO EM OPERAÇÃO COM ARQUIVOS          *'
              DISPLAY '* COMANDO: VAZIO                               *'
              DISPLAY '* ARQUIVO: ENT01105                            *'
              DISPLAY '* FILE-STATUS:' WRK-FS-ARQENT01 '              *'
              DISPLAY '* PROGRAMA EXER0105 CANCELADO                  *'
              DISPLAY '************************************************'
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF 

           WRITE FD-ARQSAI01 FROM WRK-CABEC.
           IF NOT WRK-FS-SAI01-OK 
              DISPLAY '************************************************'
              DISPLAY '*       ERRO EM OPERAÇÃO COM ARQUIVOS          *'
              DISPLAY '* COMANDO: WRITE                               *'
              DISPLAY '* ARQUIVO: SAI01105                            *'
              DISPLAY '* FILE-STATUS:' WRK-FS-ARQSAI01 '*'
              DISPLAY '* PROGRAMA EXER0105 CANCELADO                  *'
              DISPLAY '************************************************'
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF 

           .

      *
      *----------------------------------------------------------------*
       1000-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------
      *----------------------------------------------------------------*
      *    ROTINA DE PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
       3000-PROCESSAR SECTION.
      *----------------------------------------------------------------*
      *
           MOVE ARQENT01-DAT-DIA TO WRK-DIA
           MOVE ARQENT01-DAT-MES TO WRK-MES
           MOVE ARQENT01-DAT-ANO  TO WRK-ANO

           IF WRK-DATA-CONTR-INV LESS WRK-DATA-CORRENTE 
              PERFORM 3900-GRAVAR-SAIDA
           ELSE
              MOVE '.' TO ARQENT01-DAT-CNTRL(3:1)
                          ARQENT01-DAT-CNTRL(6:1)
                                          
              DISPLAY '************************************************'
              DISPLAY '* NOME: ' ARQENT01-COD-FUNCO(1:23)
              DISPLAY '* DATA: ' ARQENT01-DAT-CNTRL 
              DISPLAY '************************************************'
           END-IF

           PERFORM 3800-LER-CADASTRO 
           .
      *
      *----------------------------------------------------------------*
       3000-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    ROTINA DE LEITURA DO ARQUIVO ARQENT01
      *----------------------------------------------------------------*
       3800-LER-CADASTRO SECTION.
      *----------------------------------------------------------------*
      *
           READ ARQENT01 INTO ARQENT01-REGISTRO.
      *
           IF WRK-FS-ENT01-OK OR WRK-FS-ENT01-FIM 
              IF WRK-FS-ENT01-OK 
                 ADD 1 TO ACU-LIDOS-ARQENT01 
              END-IF 
           ELSE
              DISPLAY '************************************************'
              DISPLAY '*       ERRO EM OPERAÇÃO COM ARQUIVOS          *'
              DISPLAY '* COMANDO: READ                                *'
              DISPLAY '* ARQUIVO: ENT01105                            *'
              DISPLAY '* FILE-STATUS:' WRK-FS-ARQENT01               '*'
              DISPLAY '* PROGRAMA EXER0105 CANCELADO                  *'
              DISPLAY '************************************************'
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF
           .

      *
      *----------------------------------------------------------------*
       3800-99-FIM.                     
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------
       3900-GRAVAR-SAIDA SECTION.
      *----------------------------------------------------------------*

           MOVE ARQENT01-NOM-FUNCO TO ARQSAI01-NOM-FUNCO
           MOVE ARQENT01-NOM-ESCTO TO ARQSAI01-NOM-ESCTO
           MOVE ARQENT01-DAT-CNTRL TO ARQSAI01-DAT-CNTRL
           MOVE '.' TO ARQSAI01-DAT-CNTRL(3:1)
                       ARQSAI01-DAT-CNTRL(6:1)

           WRITE FD-ARQSAI01 FROM ARQSAI01-REGISTRO.

           IF NOT WRK-FS-SAI01-OK 
              DISPLAY '************************************************'
              DISPLAY '*       ERRO EM OPERAÇÃO COM ARQUIVOS          *'
              DISPLAY '* COMANDO: WRITE                               *'
              DISPLAY '* ARQUIVO: SAI01105                            *'
              DISPLAY '* FILE-STATUS:' WRK-FS-ARQSAI01               '*'
              DISPLAY '* PROGRAMA EXER0105 CANCELADO                  *'
              DISPLAY '************************************************'
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF 

           COMPUTE ACU-GRAVA-ARQSAI01 = ACU-GRAVA-ARQSAI01 + 1.
           
      *----------------------------------------------------------------*
       3900-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    ROTINA DE FINALIZAÇÃO
      *----------------------------------------------------------------*
       4000-FINALIZAR SECTION.
      *----------------------------------------------------------------*
            CLOSE ARQENT01  
            CLOSE ARQSAI01  
           IF NOT WRK-FS-SAI01-OK AND WRK-FS-ENT01-FIM
              DISPLAY '************************************************'
              DISPLAY '*       ERRO EM OPERACAO COM ARQUIVOS          *'
              DISPLAY '* COMANDO: CLOSE                               *'
              DISPLAY '* FILE-STATUS ENT:' WRK-FS-ARQENT01           '*'
              DISPLAY '* FILE-STATUS SAI:' WRK-FS-ARQSAI01           '*'
              DISPLAY '* PROGRAMA EXER0102 CANCELADO                  *'
              DISPLAY '************************************************'
           END-IF  
              
            DISPLAY '**************************************************'
            DISPLAY '* QTDE DE REGISTROS LIDOS:' ACU-LIDOS-ARQENT01  '*'
            DISPLAY '* QTDE DE REGISTROS GRAVADOS:'ACU-GRAVA-ARQSAI01'*'
            DISPLAY '*                                                *'
            DISPLAY '* nome-do-programa: FIM NORMAL                   *'
            DISPLAY '**************************************************'
            
           STOP RUN.
      *     END PROGRAM EXER0102.
      *----------------------------------------------------------------*
       4000-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
      **    ROTINA DE TRATAMENTO DE ERROS NOS ARQUIVOS
      **----------------------------------------------------------------*
       9100-ERROS-ARQUIVOS SECTION.
      *----------------------------------------------------------------*
           PERFORM 4000-FINALIZAR
           .
      *----------------------------------------------------------------*
       9100-99-FIM.
           EXIT.
      *----------------------------------------------------------------*