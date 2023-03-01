      *----------------------------------------------------------------*
      *      BOOK DO TREINAMENTO EM COBOL/MAINFRAME - EXERCÍCIO 1      *
      *----------------------------------------------------------------*
      * TAMANHO DO REGISTRO: 176 BYTES
      *----------------------------------------------------------------*
      * AUTOR: JULIANA SILVA SOARES
      * DATA: 06/01/2023
      *----------------------------------------------------------------*
      * NOME DO CAMPO             DESCRICAO
      * ================          =========
      * COD-FUNCO                 Código do funcionário
      * NOM-FUNCO                 Nome do funcionário
      * NOM-ESCTO                 Nome do escritório
      * COD-DEPTO                 Código do departamento
      * NOM-DEPTO                 Nome do departamento
      * DAT-CNTRL                 Data de controle interna
      *----------------------------------------------------------------*

       01 ARQENT01-REGISTRO.
             05 ARQENT01-REG-FUNCO.
                   10 ARQENT01-COD-FUNCO                  PIC 9(08).
                   10 ARQENT01-NOM-FUNCO                  PIC X(50).
                   10 ARQENT01-NOM-ESCTO                  PIC X(30).
             05 ARQENT01-REG-DEPTO.
                   10 ARQENT01-COD-DEPTO                  PIC 9(15).
                   10 ARQENT01-NOM-DEPTO                  PIC X(55).
                   10 ARQENT01-DAT-CNTRL                  PIC X(10).
             05 ARQENT01-DAT-CONVERT.
                   10 ARQENT01-DAT-DIA                    PIC X(02).
                   10 FILLER                              PIC X(01).
                   10 ARQENT01-DAT-MES                    PIC X(02).
                   10 FILLER                              PIC X(01).
                   10 ARQENT01-DAT-ANO                    PIC X(02).