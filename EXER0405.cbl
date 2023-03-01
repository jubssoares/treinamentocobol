      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. EXER0405.
       AUTHOR.     JULIANA SOARES.
      *================================================================*
      *    PROGRAMA....: EXER0405                                      *
      *    PROGRAMADOR.: JULIANA SOARES                                *
      *    DATA........: 25/01/2023                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR ARQUIVO CSV DADOS DA TABELA DB2       *
      *                    INFO_PSSOA                                  *
      *----------------------------------------------------------------*
      *    ARQUIVOS:                                                   *
      *       DDNAME                                 INCLUDE/BOOK      *
      *      ARQENT01                                  ENT04105        *
      *      ARQSAI01                                  SAI04105        *
      *    TABELAS:                                                    *
      *      INFO_PSSOA                                CADUB069        *
      *      ENDER_PSSOA                               CADUB018        *
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

           SELECT ARQSAI01 ASSIGN       TO UT-S-ARQSAI01
                      FILE STATUS       IS WRK-FS-ARQSAI01.
      *
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
      *               ORG. SEQUENCIAL   -   LRECL = 10                *
      *----------------------------------------------------------------*

       FD  ARQENT01
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS  0 RECORDS.
       01 FD-ARQENT01             PIC X(10).

      *---------------------------------------------------------------*
      *   OUTPUT:     ARQUIVO DE SAIDA                                *
      *               ORG. SEQUENCIAL   -   LRECL = 117                *
      *---------------------------------------------------------------*

       FD  ARQSAI01
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01 FD-ARQSAI01             PIC X(117).

      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *

      *----------------------------------------------------------------*
       77 FILLER                  PIC  X(050) VALUE
             'EXER0405 - INICIO DA AREA DE WORKING'.
      *----------------------------------------------------------------*
      *
       77 WRK-PROGRAMA            PIC X(008) VALUE 'EXER0405'.
       77 WRK-MASK-QTREG          PIC ZZ.ZZ9.
       77 WRK-TABELA              PIC X(010) VALUE SPACES.
       77 WRK-SQLCODE             PIC -99999.
      *
       01 WRK-ACUMULADORES.
           03 ACU-LIDOS-ARQENT01     PIC  9(005) VALUE ZEROS.
           03 ACU-DESPREZADOS        PIC  9(005) VALUE ZEROS.
           03 ACU-GRAVA-ARQSAI01     PIC  9(005) VALUE ZEROS.

       01 WRK-CABEC.
          05 WRK-CABEC-ARQSAI01   PIC  X(040) VALUE
             'COD-CLI;NOM-CLI;DATA-ATULZ;QTE-ENDER-CAD'.

       77 WRK-ARQUIVO             PIC  X(008) VALUE SPACES.
          88 WRK-NOM-ARQENT01     VALUE 'ARQENT01'.
          88 WRK-NOM-ARQSAI01     VALUE 'ARQSAI01'.

       77 WRK-COMANDO             PIC  X(005) VALUE SPACES.
          88 WRK-CM-OPEN          VALUE 'OPEN '.
          88 WRK-CM-CLOSE         VALUE 'CLOSE'.
          88 WRK-CM-READ          VALUE 'READ '.
          88 WRK-CM-WRITE         VALUE 'WRITE'.
       
       77 WRK-QTD-ENDER          PIC S9(05) COMP-3 VALUES +0.

      *----------------------------------------------------------------*
       01 FILLER                  PIC  X(050) VALUE
           'AREA PARA TRATAMENTO DE FILE-STATUS'.
      *----------------------------------------------------------------*
      *
       01 WRK-AREA-FS.
          05 WRK-FS-ARQENT01      PIC  X(002) VALUE SPACES.
             88 WRK-FS-ENT01-OK   VALUE '00'.
             88 WRK-FS-ENT01-FIM  VALUE '10'.
          05 WRK-FS-ARQSAI01      PIC  X(002) VALUE SPACES.
             88 WRK-FS-SAI01-OK   VALUE '00'.
          05 WRK-FS-DISPLAY       PIC  X(002) VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01 FILLER                  PIC  X(050) VALUE
             'AREA DOS BOOKS DOS ARQUIVOS DE ENTRADA E SAIDA'.
      *----------------------------------------------------------------*
      *
           COPY ENT04105.
           COPY SAI04105.
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
      *    DB2PRD.ENDER_PSSOA                                          *
           EXEC SQL
               INCLUDE CADUB018
           END-EXEC.
      *
      *----------------------------------------------------------------*
       01 FILLER                  PIC  X(050) VALUE
             'ENT02016 - FIM DA AREA DE WORKING'.
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
           PERFORM 3000-PROCESSAR UNTIL WRK-FS-ENT01-FIM
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
           SET  WRK-CM-OPEN            TO TRUE
           SET  WRK-NOM-ARQENT01       TO TRUE

           OPEN INPUT ARQENT01
           IF NOT WRK-FS-ENT01-OK
              PERFORM 9000-ERROS-ARQUIVOS
           END-IF

           SET  WRK-NOM-ARQSAI01       TO TRUE
           SET  WRK-CM-OPEN            TO TRUE

           OPEN OUTPUT ARQSAI01
      *
           IF NOT WRK-FS-SAI01-OK
              PERFORM 9000-ERROS-ARQUIVOS
           END-IF

           PERFORM 3800-LER-ARQENT01

           IF WRK-FS-ENT01-FIM
              DISPLAY '*****************************'
              DISPLAY '* ARQUIVO DE ENTRADA VAZIO  *'
              DISPLAY '* COMANDO: ' WRK-COMANDO
                                      '            *'
              DISPLAY '* ARQUIVO: ' WRK-ARQUIVO
                                         '         *'
              DISPLAY '* ' WRK-PROGRAMA
                                '                  *'
              DISPLAY '*****************************'
              PERFORM 4100-FINALIZAR
           END-IF

           SET WRK-CM-WRITE            TO TRUE

           WRITE FD-ARQSAI01 FROM WRK-CABEC

           IF NOT WRK-FS-SAI01-OK
              PERFORM 9000-ERROS-ARQUIVOS
           END-IF

           .
      *
      *----------------------------------------------------------------*
       1000-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    ROTINA DE PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
       3000-PROCESSAR SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 3100-SELECIONAR-CLIENTE
           PERFORM 3800-LER-ARQENT01
           .
      *
      *----------------------------------------------------------------*
       3000-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    ACESSA TABELA CLIENTES
      *----------------------------------------------------------------*
       3100-SELECIONAR-CLIENTE         SECTION.
      *

           EXEC SQL
              SELECT  IPSSOA_COPLT,
                      HULT_ATULZ
                   INTO  :CADUB069.IPSSOA-COPLT,
                         :CADUB069.HULT-ATULZ
                   FROM  DB2PRD.INFO_PSSOA
                   WHERE CCLUB = :CADUB069.CCLUB
           END-EXEC

           EVALUATE SQLCODE
               WHEN ZEROS
                    MOVE ARQENT01-COD-CLI     TO ARQSAI01-COD-CLI
                    MOVE IPSSOA-COPLT OF CADUB069 TO ARQSAI01-NOM-CLI
                    MOVE HULT-ATULZ   OF CADUB069 TO ARQSAI01-DAT-ATULZ
                    PERFORM 3200-CONTAR-ENDERECO
               WHEN +100
                    DISPLAY ARQENT01-COD-CLI ' - CLIENTE INEXISTENTE'
                    ADD 1              TO ACU-DESPREZADOS
               WHEN OTHER
                    MOVE 'INFO_PSSOA'  TO WRK-TABELA
                    MOVE SQLCODE       TO WRK-SQLCODE
                    DISPLAY '*******************************'
                    DISPLAY '*       ERRO ACESSO DB2       *'
                    DISPLAY '* TABELA : ' WRK-TABELA
                                                  '        *'
                    DISPLAY '* SQLCODE: ' WRK-SQLCODE
                                             '             *'
                    DISPLAY '* ' WRK-PROGRAMA
                                      ' CANCELADO          *'
                    DISPLAY '*******************************'

                    PERFORM 9900-ENCERRAR

           END-EVALUATE
           EXIT.
      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    ACESSA TABELA ENDERECOS                                     *
      *----------------------------------------------------------------*
       3200-CONTAR-ENDERECO         SECTION. 
      *
           EXEC SQL
              SELECT COUNT(*)
                   INTO :WRK-QTD-ENDER
                   FROM DB2PRD.ENDER_PSSOA
                   WHERE CCLUB = :CADUB069.CCLUB
           END-EXEC.

           EVALUATE SQLCODE
               WHEN ZEROS
                    MOVE WRK-QTD-ENDER TO ARQSAI01-QDT-ENDER
                    PERFORM 3900-GRAVA-SAIDA
               WHEN OTHER
                    MOVE 'ENDER_PSSOA'  TO WRK-TABELA
                    MOVE SQLCODE       TO WRK-SQLCODE
                    DISPLAY '*******************************'
                    DISPLAY '*       ERRO ACESSO DB2       *'
                    DISPLAY '* TABELA : ' WRK-TABELA
                                                  '        *'
                    DISPLAY '* SQLCODE: ' WRK-SQLCODE
                                             '             *'
                    DISPLAY '* ' WRK-PROGRAMA
                                      ' CANCELADO          *'
                    DISPLAY '*******************************'

                    PERFORM 9900-ENCERRAR

           END-EVALUATE
           EXIT.
      *
      *----------------------------------------------------------------*
      *    ROTINA DE LEITURA DO ARQUIVO ARQENT01
      *----------------------------------------------------------------*
       3800-LER-ARQENT01 SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE ARQENT01-REGISTRO

           SET WRK-CM-READ   TO TRUE

           READ ARQENT01 INTO ARQENT01-REGISTRO

           EVALUATE TRUE
               WHEN WRK-FS-ENT01-OK
                    ADD 1 TO ACU-LIDOS-ARQENT01
                    MOVE ARQENT01-COD-CLI TO CCLUB OF CADUB069
               WHEN WRK-FS-ENT01-FIM
                    CONTINUE
               WHEN OTHER
                    PERFORM 9000-ERROS-ARQUIVOS
           END-EVALUATE
           .
      *
      *----------------------------------------------------------------*
       3800-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3900-GRAVA-SAIDA SECTION.
      *----------------------------------------------------------------*
      *
           SET WRK-CM-WRITE  TO TRUE

           WRITE FD-ARQSAI01 FROM ARQSAI01-REGISTRO

           IF NOT WRK-FS-SAI01-OK
              PERFORM 9000-ERROS-ARQUIVOS
           END-IF

           ADD 1             TO ACU-GRAVA-ARQSAI01

           INITIALIZE ARQSAI01-REGISTRO
           .
      *
      *----------------------------------------------------------------*
       3900-99-FIM.
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
      *    ROTINA DE FINALIZACAO DO PROGRAMA
      *----------------------------------------------------------------*
       4100-FINALIZAR SECTION.
      *----------------------------------------------------------------*
           SET WRK-CM-CLOSE TO TRUE.

           CLOSE ARQENT01.
           IF NOT WRK-FS-ENT01-OK
              PERFORM 9000-ERROS-ARQUIVOS
           END-IF

           CLOSE ARQSAI01.
           IF NOT WRK-FS-SAI01-OK
              PERFORM 9000-ERROS-ARQUIVOS
           END-IF

           DISPLAY '***************************************************'
           MOVE ACU-LIDOS-ARQENT01     TO WRK-MASK-QTREG
           DISPLAY '* QTDE REGISTROS LIDOS   : ' WRK-MASK-QTREG
                                                    '                 *'
           MOVE ACU-GRAVA-ARQSAI01     TO WRK-MASK-QTREG
           DISPLAY '* QTDE REGISTROS GRAVADOS: ' WRK-MASK-QTREG
                                                    '                 *'
           MOVE ACU-DESPREZADOS        TO WRK-MASK-QTREG
           DISPLAY '* QTDE DESPREZADOS       : ' WRK-MASK-QTREG
                                                    '                 *'
           DISPLAY '*                                                 *'
           DISPLAY '* ' WRK-PROGRAMA
                             ' FIM NORMAL                             *'
           DISPLAY '***************************************************'

           PERFORM 9900-ENCERRAR
           .
      *
      *----------------------------------------------------------------*
      *    ROTINA DE FINALIZACAO DO PROGRAMA
      *----------------------------------------------------------------*
       9900-ENCERRAR SECTION.
      *----------------------------------------------------------------*
           STOP RUN.