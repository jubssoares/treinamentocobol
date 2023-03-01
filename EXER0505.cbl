      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. EXER0505.
       AUTHOR.     JULIANA SOARES.
      *================================================================*
      *    PROGRAMA....: EXER0505                                      *
      *    PROGRAMADOR.: JULIANA SOARES                                *
      *    DATA........: 27/01/2023                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CRIAR CURSOR PARA FAZER LEITURA DE TABELA   *
      *                    DB2.                                        *
      *----------------------------------------------------------------*
      *    ARQUIVOS:                                                   *
      *       DDNAME                                 INCLUDE/BOOK      *        
      *      ARQSAI01                                ------------      *        
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
           SELECT ARQSAI01 ASSIGN       TO UT-S-ARQSAI01
                      FILE STATUS       IS WRK-FS-ARQSAI01.
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *                                                                *
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *              ARQUIVO DOS REGISTROS DE SAIDA                    *
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *   OUTPUT:     ARQUIVO DE SAIDA                                 *
      *               ORG. SEQUENCIAL   -   LRECL = 155                *
      *----------------------------------------------------------------*

       FD  ARQSAI01
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01 FD-ARQSAI01             PIC X(155).

      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *

      *----------------------------------------------------------------*
       77 FILLER                  PIC  X(050) VALUE
             'EXER0505 - INICIO DA AREA DE WORKING'.
      *----------------------------------------------------------------*
      *
       77 WRK-PROGRAMA            PIC X(008) VALUE 'EXER0405'.
       77 WRK-MASK-QTREG          PIC ZZ.ZZ9.
       77 WRK-TABELA              PIC X(010) VALUE SPACES.
       77 WRK-SQLCODE             PIC -99999.
      *
       01 WRK-ACUMULADORES.
           03 ACU-LIDOS              PIC 9(005) VALUE ZEROS.
           03 ACU-DESPREZADOS        PIC 9(005) VALUE ZEROS.
           03 ACU-GRAVA-ARQSAI01     PIC 9(005) VALUE ZEROS.

       01 WRK-CABEC.
          05 WRK-CABEC-ARQSAI01   PIC  X(041) VALUE
              'CODIGO-CLIENTE;NOME-CLIENTE;EMAIL-CLIENTE'.

       77 WRK-ARQUIVO             PIC  X(008) VALUE SPACES.
          88 WRK-NOM-ARQSAI01     VALUE 'ARQSAI01'.
       
       77 WRK-COMANDO             PIC  X(005) VALUE SPACES.
          88 WRK-CM-OPEN          VALUE 'OPEN '.
          88 WRK-CM-CLOSE         VALUE 'CLOSE'.
          88 WRK-CM-READ          VALUE 'READ '.
          88 WRK-CM-WRITE         VALUE 'WRITE'.
       
       77  WRK-FIM-CSR1           PIC  X(001)     VALUE SPACES.

      * 
      *----------------------------------------------------------------*
       01 FILLER                  PIC  X(050) VALUE
           'AREA PARA TRATAMENTO DE FILE-STATUS'.
      *----------------------------------------------------------------*
      *
       01 WRK-AREA-FS.
          05 WRK-FS-ARQSAI01      PIC  X(002) VALUE SPACES.
             88 WRK-FS-SAI01-OK   VALUE '00'.
          05 WRK-FS-DISPLAY       PIC  X(002) VALUE SPACES.
      
      *----------------------------------------------------------------*
       01 FILLER                  PIC  X(040) VALUE
             'AREA DOS BOOKS DOS ARQUIVOS DE SAIDA'.
      *----------------------------------------------------------------*
      *
       01 ARQSAI01-REGISTRO.
           03 AQRSAI01-COD-CLIENTE             PIC 9.999.999.999.
           03 FILLER                           PIC X(001) VALUE ';'.
           03 ARQSAI01-NOM-CLIENTE             PIC X(070) VALUE SPACES.
           03 FILLER                           PIC X(001) VALUE ';'.
           03 AQRSAI01-EML-CLIENTE             PIC X(070) VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(013)     VALUE
           'AREA PARA DB2'.
      *----------------------------------------------------------------*
      *
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
      *
      *    DB2PRD.INFO_PSSOA                                           *
           EXEC SQL
               INCLUDE CADUB069
           END-EXEC.
      *
           EXEC SQL DECLARE CSR-B069 CURSOR WITH HOLD FOR
              SELECT CCLUB, IPSSOA_COPLT, EEMAIL_PSSOA
                  FROM DB2PRD.INFO_PSSOA
                  WHERE CSGL_UF            = 'BA'
                  AND   CID_TPO_PSSOA      = 'F'
                  AND   CSEXO              = 'F'
                  AND   CPTCAO_ESPAC_TBELA = 1
                  AND   EEMAIL_PSSOA IS NOT NULL
              ORDER BY CCLUB
           END-EXEC

      *----------------------------------------------------------------*
       01 FILLER                  PIC  X(050) VALUE
             'EXER0505 - FIM DA AREA DE WORKING'.
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
           CALL 'CKRS1000'
           CALL 'CKRS1050'

           PERFORM 1000-INICIALIZAR
      *
           PERFORM 3000-PROCESSAR UNTIL WRK-FIM-CSR1 EQUAL 'S'
      *
           PERFORM 4100-FINALIZAR
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
           SET  WRK-NOM-ARQSAI01       TO TRUE
           SET  WRK-CM-OPEN            TO TRUE

           OPEN OUTPUT ARQSAI01
      *
           EXEC SQL
               OPEN CSR-B069
           END-EXEC

           EVALUATE SQLCODE
               WHEN ZEROS
                   CONTINUE
               WHEN OTHER
                    MOVE 'INFO_PSSOA'   TO WRK-TABELA
                    MOVE SQLCODE        TO WRK-SQLCODE
                    PERFORM 9000-ERROS-ARQUIVOS
           END-EVALUATE
      *

           IF NOT WRK-FS-SAI01-OK
              PERFORM 9000-ERROS-ARQUIVOS
           END-IF

           

           SET WRK-CM-WRITE            TO TRUE

           WRITE FD-ARQSAI01 FROM WRK-CABEC

           IF NOT WRK-FS-SAI01-OK
              PERFORM 9000-ERROS-ARQUIVOS
           END-IF

           PERFORM 3800-LER-CURSOR
           .
      *
      *----------------------------------------------------------------*
       1000-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
      
      *
      *----------------------------------------------------------------*
      *    ROTINA DE LEITURA DO CURSOR                                 *
      *----------------------------------------------------------------*
       3800-LER-CURSOR                 SECTION.
      *----------------------------------------------------------------*
      *
           EXEC SQL
               FETCH CSR-B069 INTO
                       :CADUB069.CCLUB 
                      ,:CADUB069.IPSSOA-COPLT
                      ,:CADUB069.EEMAIL-PSSOA
           END-EXEC
      *
           EVALUATE TRUE
               WHEN SQLCODE EQUAL +100
                    MOVE 'S'           TO WRK-FIM-CSR1
               WHEN SQLCODE EQUAL ZEROS
                    ADD 1              TO ACU-LIDOS
               WHEN OTHER
                    PERFORM 9300-ERRO-DB2
           END-EVALUATE
      *
           .
      *
      *----------------------------------------------------------------*
       3800-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA DE PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
       3000-PROCESSAR SECTION.
      *----------------------------------------------------------------*
      *
           MOVE CCLUB            TO AQRSAI01-COD-CLIENTE
           MOVE IPSSOA-COPLT     TO ARQSAI01-NOM-CLIENTE
           MOVE EEMAIL-PSSOA     TO AQRSAI01-EML-CLIENTE

           WRITE FD-ARQSAI01     FROM ARQSAI01-REGISTRO

           IF NOT WRK-FS-SAI01-OK 
              PERFORM 9000-ERROS-ARQUIVOS
           END-IF

           ADD 1 TO ACU-GRAVA-ARQSAI01
           PERFORM 3800-LER-CURSOR
           
           .
      *
      *----------------------------------------------------------------*
       3000-99-FIM.
           EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE TRATAMENTO DE ERRO
      *----------------------------------------------------------------*
       9000-ERROS-ARQUIVOS SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*****************************'
           DISPLAY '*     ERRO COM ARQUIVOS     *'
           DISPLAY '* COMANDO: ' WRK-COMANDO
                                   '            *'
           DISPLAY '* ARQUIVO: ' WRK-ARQUIVO
                                      '         *'
           DISPLAY '* ' WRK-PROGRAMA
                             ' CANCELADO        *'
           DISPLAY '*****************************'

           PERFORM 9900-ENCERRAR
           .
      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    ROTINA DE FINALIZACAO DO PROGRAMA
      *----------------------------------------------------------------*
       4100-FINALIZAR SECTION.
      *----------------------------------------------------------------*
           SET WRK-CM-CLOSE TO TRUE.

           CLOSE ARQSAI01.
           IF NOT WRK-FS-SAI01-OK
              PERFORM 9000-ERROS-ARQUIVOS
           END-IF

           EXEC SQL 
                CLOSE CSR-B069
           END-EXEC

           IF SQLCODE NOT EQUAL +0
              PERFORM 9300-ERRO-DB2
           END-IF

           DISPLAY '***************************************************'
           MOVE ACU-GRAVA-ARQSAI01     TO WRK-MASK-QTREG
           DISPLAY '* QTDE REGISTROS GRAVADOS: ' WRK-MASK-QTREG
                                                    '                 *'
           MOVE ACU-LIDOS              TO WRK-MASK-QTREG
           DISPLAY '* QTDE LIDOS             : ' WRK-MASK-QTREG
                                                    '                 *'
           DISPLAY '*                                                 *'
           DISPLAY '* ' WRK-PROGRAMA
                             ' FIM NORMAL                             *'
           DISPLAY '***************************************************'

           PERFORM 9900-ENCERRAR
           .
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO DB2                          *
      *----------------------------------------------------------------*
       9300-ERRO-DB2                   SECTION.
      *----------------------------------------------------------------*
      
           DISPLAY '*****************************'
           DISPLAY '*        ERRO DE DB2        *'
           DISPLAY '* COMANDO : ' WRK-COMANDO
                                   '            *'
           DISPLAY '* ARQUIVO : ' WRK-ARQUIVO
                                      '         *'
            DISPLAY '* SQLCODE: ' WRK-SQLCODE
                                      '         *'
           DISPLAY '* ' WRK-PROGRAMA
                             ' CANCELADO        *'
           DISPLAY '*****************************'

           PERFORM 9900-ENCERRAR
           .
      *
      *----------------------------------------------------------------*
       9300-99-FIM.
           EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE FINALIZACAO DO PROGRAMA
      *----------------------------------------------------------------*
       9900-ENCERRAR SECTION.
      *----------------------------------------------------------------*
           STOP RUN.