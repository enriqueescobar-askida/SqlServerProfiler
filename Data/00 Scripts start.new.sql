SELECT * FROM HS_ProprietaireAdhere
SELECT COUNT(*) FROM INFORMATION_SCHEMA.COLUMNS  WHERE TABLE_NAME = ''gmp_systeme''
SELECT isNewTaxCalc FROM gmp_systeme
INSERT INTO HS_ArchivageLogEtape (Date_Log, Objet, Description, Usager_ID, Number, Parametres, Source) VALUES (CONVERT(DATETIME,'2016-11-10 08:07:23'), 'AgentArchivage', '***** DÉBUT de l''agent Archivage *****', 'sa', '', '', '')
SELECT CutOffTime FROM hs_ArchSchedule
INSERT INTO HS_ArchivageLogEtape (Date_Log, Objet, Description, Usager_ID, Number, Parametres, Source) VALUES (CONVERT(DATETIME,'2016-11-10 08:07:23'), 'AgentArchivage', 'Cutoff time expiré, fermeture de l''agent', 'sa', '', '', '')
SELECT MIN(SOURCE) FROM hs_ArchivageLogEtape
SELECT Arch_LastCR FROM gmp_Systeme
SELECT COUNT(Date_Log) FROM hs_ArchivageLogEtape
SELECT TOP 1 Date_Log FROM hs_ArchivageLogEtape
SELECT RTRIM(Courriel) Courriel  FROM GMP_USAGERS WHERE
SELECT Description, Parametres, SUM(CONVERT(BIGINT, Number)) AS NbTrans From hs_ArchivageLogEtape
SELECT * FROM GMP_USAGERS WITH(NOLOCK)
INSERT INTO GMP_COURRIELS (To_Address, From_Address, Subject, Body, Request_Date,Piece_Jointe, envoyer)
***** Consultez le log hs_ArchivageLogEtape pour plus d''infos.  *****
***** Voir les colonnes gmp_systeme.Arch_*  pour les paramètres *****
INSERT INTO GMP_COURRIELS (To_Address, From_Address, Subject, Body, Request_Date,Piece_Jointe, envoyer)