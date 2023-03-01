      *----------------------------------------------------------------*
      *      BOOK DO TREINAMENTO EM COBOL/MAINFRAME - EXERCÍCIO 1      *
      *----------------------------------------------------------------*
      * TAMANHO DO REGISTRO: 98 BYTES
      *----------------------------------------------------------------*
      * AUTOR: JULIANA SILVA SOARES
      * DATA: 11/01/2023
      *----------------------------------------------------------------*
      * NOME DO CAMPO             DESCRICAO
      * ================          =========
      * NOM-FUNCO                 Nome do funcionário
      * NOM-ESCTO                 Nome do escritório
      * DAT-CNTRL                 Data de controle interna
      *----------------------------------------------------------------*

       01 ARQSAI01-REGISTRO.
             05 ARQSAI01-REG-FUNCO.
                   10 ARQSAI01-NOM-FUNCO                  PIC X(50).
                   10 ARQSAI01-NOM-ESCTO                  PIC X(30).
             05 ARQSAI01-REG-DEPTO.
                   10 ARQSAI01-DAT-CNTRL                  PIC X(08).
             05 ARQSAI01-DAT-CONV.
                   10 ARQSAI01-DAT-DIA                    PIC X(02).
                   10 FILLER                              PIC X(01).
                   10 ARQSAI01-DAT-MES                    PIC X(02).
                   10 FILLER                              PIC X(01).
                   10 ARQSAI01-DAT-ANO                    PIC X(02).