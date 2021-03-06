SELECT * FROM GMP_USAGERS WITH(NOLOCK)
select ID_VILLE, lTrim(rTrim(NOM_VILLE)) AS NOM_VILLE from GMP_VILLE  inner join GMP_PROVINCE on GMP_VILLE.ID_PROVINCE = GMP_PROVINCE.ID_Province  inner join GMP_PAYS on GMP_VILLE.ID_PAYS = GMP_PAYS.ID_PAYS
SELECT CategorieID, Description FROM GMP_CATEGORIEPROBLEME
select Group_Partenaire from GMP_USAGERS where UsagerID = ''ed50617''
SELECT DISTINCT UsagerID,  FName + '' '' + LName +''('' + UsagerID + '')'' as Usager FROM GMP_USAGERS  WHERE Inactif = ''0''  ORDER BY UsagerID
SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''
SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''38'' AND USER_CLASS=''ADMIN''
exec sp_cursoropen @p1 output,N'SELECT SV00515.Contract_Internal_Name FROM SV00515  LEFT JOIN GMP_SV00515  ON SV00515.Contract_Internal_Name = GMP_SV00515.Contract_Internal_Name  WHERE (GMP_SV00515.Desuet = ''0''  AND DateFin >= Convert(DateTime,getdate(),112)) OR SV00515.Contract_Internal_Name = ''''  ORDER BY SV00515.Contract_Internal_Name ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT SV00310.Type_of_Problem AS Type_of_Problem FROM SV00310  LEFT JOIN GMP_SV00310  ON SV00310.Type_of_Problem = GMP_SV00310.Type_of_Problem  WHERE 1 = 1  ORDER BY 1 ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' select distinct COMPAGNIE, Max(DEPT_ID) DEPT_ID from GMP_DEPARTEMENT group by COMPAGNIE ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' select Group_Partenaire from GMP_USAGERS where UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' select Task_Code,''(''+rtrim(Task_Code)+'')''+Task_Description AS Task_Description from GMP_SV00310 a inner join SV00560 b on a.Type_of_Problem = b.Equipment_Type where 1=1 ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT     CLASSEMENT, DESCRIPTION From GMP_ORDERBY  order  by DESCRIPTION ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' select Group_Partenaire from GMP_USAGERS where UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' Select  * from GMP_PARTENAIRES  where Traitement=''1'' and ID_Partenaire <>''''  and Resolution_Code<>''''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_USAGERS WITH(NOLOCK) WHERE UsagerID = ''ed50617'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''31'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT DISTINCT UsagerID, FName + '' '' + LName FROM GMP_USAGERS  WHERE Inactif = ''0''  ORDER BY FName + '' '' + LName ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_USAGERS WITH(NOLOCK) WHERE UsagerID = ''ed50617'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_USAGERS WITH(NOLOCK) WHERE UsagerID = ''ed50617'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_USAGERS WITH(NOLOCK) WHERE UsagerID = ''ed50617'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_USAGERS WITH(NOLOCK) WHERE UsagerID = ''6286'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_USAGERS WITH(NOLOCK) WHERE UsagerID = ''6286'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_USAGERS WITH(NOLOCK) WHERE UsagerID = ''ed00009'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_USAGERS WITH(NOLOCK) WHERE UsagerID = ''ed00009'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_USAGERS  WHERE UsagerID = ''ed00009'' ',@p3 output,@p4 output,@p5 output
UPDATE GMP_USAGERS SET  Courriel = 'edjuikomn@HydroSolution.com',  FName = 'Erick',  LName = 'Dion',  Mobile_Pwd = '',  Security_Group = 'ADMIN',  AdminEntretien = '0',  Rap_Script = '1'  , Telephone= '' , SiPeutUtiliserListeTache= 1 , SiPeutRembourserCC= 1 , MaximumCredit= 88888 , SiPeutAjouterModifierRabais = 1 WHERE UsagerID = 'ed00009'
UPDATE GMP_USAGERS SET  Courriel = 'edjuikomn@HydroSolution.com',  FName = 'Erick',  LName = 'Dion',  Mobile_Pwd = '',  Security_Group = 'ADMIN',  AdminEntretien = '0',  Rap_Script = '1'  , Telephone= '' , SiPeutUtiliserListeTache= 1 , SiPeutRembourserCC= 1 , MaximumCredit= 88888 , SiPeutAjouterModifierRabais = 1 WHERE UsagerID = 'ed00009'
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT SV00515.Contract_Internal_Name  FROM SV00515  LEFT JOIN GMP_SV00515  ON SV00515.Contract_Internal_Name = GMP_SV00515.Contract_Internal_Name ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT RTrim(TypeTarifID) TypeTarifID, RTrim(Description) Description, Actif FROM GMP_TYPESTARIFS tt WHERE EstSPA= 0 AND (Actif = ''1'' OR (DateEffectif <= GETDATE() AND DateInactif >= GETDATE()) ) group by TypeTarifID, Description, Actif ORDER BY Max(DateEffectif) desc, TypeTarifID DESC ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT DISTINCT Sub_4, CAST(Sub_4 AS int) as Sub_4Num  FROM SV00405 ORDER BY CAST(sub_4 AS int) ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT DISTINCT TAR_CD_IDENTIFICATION FROM GMP_TARIF ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT DISTINCT Manufacturer_ID,Manufacturer_Description FROM SV00410  ORDER BY Manufacturer_ID ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT DISTINCT ID,MOD_NOM FROM GMP_MODELE  ORDER BY MOD_NOM ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_TARIF  ORDER BY  TAR_ID ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_TARIF  WHERE TAR_ID = ''1'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_TARIF  WHERE TAR_ID = ''7'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT MAX(TAR_ID) FROM GMP_TARIF ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_TARIF  WHERE TAR_ID = ''4128'' ',@p3 output,@p4 output,@p5 output
INSERT INTO GMP_TARIF (TAR_PRO_CD_PRODUIT,COL002,TAR_COA_CODE,  TAR_CD_IDENTIFICATION,TAR_DT_START,TAR_DT_FINISH,TAR_ANNUAL_PRICE,  TAR_MARQUE,TAR_MODELE )  VALUES ('PG1','R','244','',CONVERT(DATETIME,'2016-11-10'),CONVERT(DATETIME,'2016-11-10'),'1000','','' )
INSERT INTO GMP_TARIF (TAR_PRO_CD_PRODUIT,COL002,TAR_COA_CODE,  TAR_CD_IDENTIFICATION,TAR_DT_START,TAR_DT_FINISH,TAR_ANNUAL_PRICE,  TAR_MARQUE,TAR_MODELE )  VALUES ('PG1','R','244','',CONVERT(DATETIME,'2016-11-10'),CONVERT(DATETIME,'2016-11-10'),'1000','','' )
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_TARIF  ORDER BY  TAR_ID ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_TARIF  WHERE TAR_ID = ''7'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_TARIF  WHERE TAR_ID = ''1'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' Select  * from GMP_PARTENAIRES  where Traitement=''1'' and ID_Partenaire <>''''  and Resolution_Code<>''''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'select Description, promotionID from gmp_rabais where DateFin >= CONVERT(DATETIME,''2016-11-10'')order by description',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'select Sub_4,Equipment_Type,Sub_3,Manufacturer_ID, estVenteSeulement from SV00405 Where Historique = 0 and Usager = 0  AND EstCVAC= 0 AND EstSPA= 0 AND EstNHS = 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'select * from gmp_rabais where promotionID = ''P0000089''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'select divisionvente, divisionlocation from gmp_SV00310 where Problem_ID = 556 and Type_of_Problem = ''ACH40G40$''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_RABAISWEB_EQUIPEMENT WHERE PromotionID = ''P0000089''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'select * from GMP_RABAIS_TYPE where promotionID = ''P0000089''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'select GMP_RABAIS.description, GMP_RABAIS_LIEN.datedebut, GMP_RABAIS_LIEN.datefin, PromotionID2 from GMP_RABAIS_LIEN inner join GMP_RABAIS on GMP_RABAIS.promotionID = GMP_RABAIS_LIEN.promotionID2 Where promotionID1 = ''P0000089'' And promotionID2 <> ''P0000089''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'select Description, promotionID from gmp_rabais where DateFin >= CONVERT(DATETIME,''2016-11-10'')order by description',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'select Sub_4,Equipment_Type,Sub_3,Manufacturer_ID, estVenteSeulement from SV00405 Where Historique = 0 and Usager = 0  AND EstCVAC= 0 AND EstSPA= 0 AND EstNHS = 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Distinct RTrim(TypeTarifID) TypeTarifID, RTrim(Description) Description, DateEffectif FROM GMP_TYPESTARIFS tt INNER JOIN GMP_TARIF t on t.COL002= tt.TypeTarifID    AND TAR_COA_CODE IN (''446'') WHERE EstSPA= 0 AND (Actif = ''1'') ORDER BY DateEffectif',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Distinct RTrim(TypeTarifID) TypeTarifID, RTrim(Description) Description, DateEffectif FROM GMP_TYPESTARIFS tt INNER JOIN GMP_TARIF t on t.COL002= tt.TypeTarifID    AND TAR_COA_CODE IN (''446'', ''249'') WHERE EstSPA= 0 AND (Actif = ''1'') ORDER BY DateEffectif',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Distinct RTrim(TypeTarifID) TypeTarifID, RTrim(Description) Description, DateEffectif FROM GMP_TYPESTARIFS tt INNER JOIN GMP_TARIF t on t.COL002= tt.TypeTarifID    AND TAR_COA_CODE IN (''446'', ''249'', ''246'') WHERE EstSPA= 0 AND (Actif = ''1'') ORDER BY DateEffectif',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Distinct RTrim(TypeTarifID) TypeTarifID, RTrim(Description) Description, DateEffectif FROM GMP_TYPESTARIFS tt INNER JOIN GMP_TARIF t on t.COL002= tt.TypeTarifID    AND TAR_COA_CODE IN (''446'', ''249'', ''246'', ''244'') WHERE EstSPA= 0 AND (Actif = ''1'') ORDER BY DateEffectif',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT t1.TaxDtlID tTax, t2.TxDtlPct PctTax FROM tx00102 t1     INNER JOIN tx00201 t2 on t1.TaxDtlID = t2.TaxDtlID WHERE TaxSchid= ''TPS-TVQ/V'' ORDER BY t2.Dex_Row_ID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'select Description, promotionID from gmp_rabais where DateFin >= CONVERT(DATETIME,''2016-11-10'')order by description',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'select Sub_4,Equipment_Type,Sub_3,Manufacturer_ID, estVenteSeulement from SV00405 Where Historique = 0 and Usager = 0  AND EstCVAC= 0 AND EstSPA= 0 AND EstNHS = 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'select * from gmp_rabais where promotionID = ''P0000087''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'select divisionvente, divisionlocation from gmp_SV00310 where Problem_ID = 556 and Type_of_Problem = ''WEB40G''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_RABAISWEB_EQUIPEMENT WHERE PromotionID = ''P0000087''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Distinct RTrim(TypeTarifID) TypeTarifID, RTrim(Description) Description, DateEffectif FROM GMP_TYPESTARIFS tt INNER JOIN GMP_TARIF t on t.COL002= tt.TypeTarifID    AND TAR_COA_CODE IN (''244'') WHERE EstSPA= 0 AND (Actif = ''1'') ORDER BY DateEffectif',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'select * from GMP_RABAIS_TYPE where promotionID = ''P0000087''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'select GMP_RABAIS.description, GMP_RABAIS_LIEN.datedebut, GMP_RABAIS_LIEN.datefin, PromotionID2 from GMP_RABAIS_LIEN inner join GMP_RABAIS on GMP_RABAIS.promotionID = GMP_RABAIS_LIEN.promotionID2 Where promotionID1 = ''P0000087'' And promotionID2 <> ''P0000087''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'select * from gmp_rabais where promotionID = ''''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'select Description, promotionID from gmp_rabais where DateFin >= CONVERT(DATETIME,''2016-11-10'')order by description',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'select Sub_4,Equipment_Type,Sub_3,Manufacturer_ID, estVenteSeulement from SV00405 Where Historique = 0 and Usager = 0  AND EstCVAC= 0 AND EstSPA= 0 AND EstNHS = 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' Select  * from GMP_PARTENAIRES  where Traitement=''1'' and ID_Partenaire <>''''  and Resolution_Code<>''''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' select ID_FONT,FONT from GMP_FONT ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' select ID_SIZE,FONTSIZE from GMP_FONTSIZE ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' select ID_STYLE,STYLE from GMP_FONTSTYLE ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' select ID_FONTCOLOR,FONTCOLOR from GMP_FONTCOLOR ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' select ID_FONTCOLOR,FONTCOLOR from GMP_FONTCOLOR ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' select * from GMP_COMMUNICATION  left join gmp_usagers on Editeur = Usagerid',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' select * from GMP_COMMUNICATION Where id_message = 61',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' select * from GMP_COMMUNICATION Where id_message = 61',@p3 output,@p4 output,@p5 output
DELETE FROM HS_COMMUNICATION_USAGERS  WHERE IDMessage = 61
DELETE FROM GMP_COMMUNICATION  WHERE ID_Message = 61
exec sp_cursoropen @p1 output,N' select * from GMP_COMMUNICATION  left join gmp_usagers on Editeur = Usagerid',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'select max(id_message) + 1 from gmp_communication',@p3 output,@p4 output,@p5 output
insert into GMP_COMMUNICATION values ( 192 ,'TEST ASKIDA SUP SAC' ,0 ,20 ,'Arial' ,'REGULAR' ,7 ,0 ,'ed50617' ,CONVERT(DATETIME,'2016-11-10 13:40:31') )
INSERT INTO HS_COMMUNICATION_USAGERS (IDMessage, UsagerID) SELECT 192, UsagerID FROM GMP_USAGERS WHERE Inactif = 0
UPDATE GMP_USAGERS  set SiMessage = 0
UPDATE GMP_USAGERS  SET SiMessage = 1
exec sp_cursoropen @p1 output,N' select * from GMP_COMMUNICATION  left join gmp_usagers on Editeur = Usagerid',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT SV00515.Contract_Internal_Name  FROM SV00515  LEFT JOIN GMP_SV00515  ON SV00515.Contract_Internal_Name = GMP_SV00515.Contract_Internal_Name ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT RTrim(TypeTarifID) TypeTarifID, RTrim(Description) Description, Actif FROM GMP_TYPESTARIFS tt WHERE EstSPA= 0 AND (Actif = ''1'' OR (DateEffectif <= GETDATE() AND DateInactif >= GETDATE()) ) group by TypeTarifID, Description, Actif ORDER BY Max(DateEffectif) desc, TypeTarifID DESC ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT DISTINCT Sub_4, CAST(Sub_4 AS int) as Sub_4Num  FROM SV00405 ORDER BY CAST(sub_4 AS int) ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT DISTINCT TAR_CD_IDENTIFICATION FROM GMP_TARIF ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT DISTINCT Manufacturer_ID,Manufacturer_Description FROM SV00410  ORDER BY Manufacturer_ID ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT DISTINCT ID,MOD_NOM FROM GMP_MODELE  ORDER BY MOD_NOM ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_TARIF  ORDER BY  TAR_ID ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_TARIF  WHERE TAR_ID = ''1'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT UPPER(LName + '' '' + FName) + ''('' + UsagerID + '')'' as UID FROM Gmp_Usagers WHERE UsagerID <> '''' AND UsagerID IN (SELECT DISTINCT UsagerResponsable FROM hs_Liste_tache) ORDER BY 1 ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT RTRIM(Type_of_Problem) Type_of_Problem, COUNT(*) as NbTachesOuvertes FROM HS_Liste_Tache hlt WHERE UsagerResponsable = '''' GROUP BY RTRIM(Type_of_Problem) ORDER BY 1',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ISNULL(Regroupement, ''[AUCUN]'') as Regroupement FROM HS_Liste_Tache hlt   LEFT JOIN hs_Regroupement r on hlt.RegroupementID= r.ID WHERE UsagerResponsable = '''' GROUP BY Regroupement ORDER BY 1',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT RTRIM(Task_Description) + ''('' + RTRIM(Task_Code) + '')'' as Tache, RTRIM(Task_Code) as Task_Code, COUNT(*) as NbTachesOuvertes FROM HS_Liste_Tache hlt   LEFT JOIN hs_Regroupement r on hlt.Regroupementid= r.ID WHERE UsagerResponsable = '''' GROUP BY RTRIM(Task_Description) + ''('' + RTRIM(Task_Code) + '')'', RTRIM(Task_Code) ORDER BY 1',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT DISTINCT ''[GROUPE] '' + Groupe as Groupe FROM hs_groupedelegation  ORDER BY 1',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT UPPER(LName + '' '' + FName) + ''('' + UsagerID + '')'' as UID FROM Gmp_Usagers WHERE UsagerID <> ''''    AND Inactif= 0   AND UsagerID not in (''sa'', ''sadev'', ''scripths'') ORDER BY 1 ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT RTRIM(Type_of_Problem) Type_of_Problem, COUNT(*) as NbTachesOuvertes FROM HS_Liste_Tache hlt WHERE UsagerResponsable = ''jdbahtahe'' GROUP BY RTRIM(Type_of_Problem) ORDER BY 1',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ISNULL(Regroupement, ''[AUCUN]'') as Regroupement FROM HS_Liste_Tache hlt   LEFT JOIN hs_Regroupement r on hlt.RegroupementID= r.ID WHERE UsagerResponsable = ''jdbahtahe'' GROUP BY Regroupement ORDER BY 1',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT RTRIM(Task_Description) + ''
('' + RTRIM(Task_Code) + '')'' as Tache, RTRIM(Task_Code) as Task_Code, COUNT(*) as NbTachesOuvertes FROM HS_Liste_Tache hlt   LEFT JOIN hs_Regroupement r on hlt.Regroupementid= r.ID WHERE UsagerResponsable = ''jdbahtahe'' GROUP BY RTRIM(Task_Description) + ''
('' + RTRIM(Task_Code) + '')'', RTRIM(Task_Code) ORDER BY 1',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT DISTINCT ''[GROUPE] '' + Groupe as Groupe FROM hs_groupedelegation  ORDER BY 1',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT UPPER(LName + '' '' + FName) + ''
('' + UsagerID + '')'' as UID FROM Gmp_Usagers WHERE UsagerID <> ''''    AND Inactif= 0   AND UsagerID not in (''sa'', ''sadev'', ''scripths'')   AND UsagerID <> ''jdbahtahe'' ORDER BY 1 ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT RTRIM(t.Task_Description) + ''
('' + RTRIM(t.Task_Code) + '')'' as Tache   , COUNT(hltD.Task_Code) as NbTachesOuvertesD FROM SV00560 t   LEFT JOIN HS_Liste_Tache hltD on t.Task_Code= hltD.Task_Code       AND hltD.UsagerResponsable = ''nb50642V''  WHERE t.Task_Code in (''FA0015'', ''FA0012'', ''FA0023'', ''FA0024'', ''FA0018'') GROUP BY RTRIM(t.Task_Description) + ''
('' + RTRIM(t.Task_Code) + '')'' ORDER BY 1',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT CustNmbr, AdrsCode, Service_Call_ID, Task_Code, SEQNUMBR FROM Hs_Liste_Tache hlt   LEFT JOIN hs_Regroupement r on hlt.Regroupementid= r.ID WHERE UsagerResponsable= ''jdbahtahe'' AND Task_Code in (''FA0015'', ''FA0012'', ''FA0023'', ''FA0024'', ''FA0018'') ORDER BY DateMinimale ASC',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT *, IsNull(GMP_POSTAL.VenteSeul, 0) as VenteSeulement, IsNull(GMP_POSTAL.PasService, 1) as PasService, IsNull(GMP_POSTAL.SiSPA, 0) as SiSPA FROM HS_Client WITH(NOLOCK) LEFT JOIN GMP_POSTAL ON GMP_POSTAL.Postal = left(REPLACE(HS_Client.ZIP, '' '', ''''), (select max(LEN(Postal)) from GMP_POSTAL Where Postal = LEFT(REPLACE(HS_Client.ZIP, '' '', ''''), LEN(gmp_Postal.Postal)))) WHERE HS_Client.CUSTNMBR = ''CL00471579     ''  AND HS_Client.ADRSCODE = ''SERVICE        '' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'  SELECT (AGPERAMT_2+AGPERAMT_3) as Tot3089, (AGPERAMT_4+AGPERAMT_5+AGPERAMT_6+AGPERAMT_7) as Tot90Plus  FROM HYSEC..RM00103 WHERE CUSTNMBR=''CR00497237''     AND CUSTBLNC > 0 ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' select CUSTNMBR from HS_CHANGEMENT_ADRESSE  where CUSTNMBR =''CL00471579''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT HS_Appel.*, HS_Appel.*, HS_Appel.Service_Call_ID as Service_Call_ID, isnull(sv00310.Problem_ID, '''') as Problem_ID FROM HS_Appel WITH(NOLOCK) LEFT JOIN SV00310 WITH(NOLOCK) ON sv00310.Type_of_Problem= HS_Appel.Type_of_Problem WHERE HS_Appel.Service_Call_ID = ''160916-2106      '' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Task_Code FROM HS_Tache  WHERE Service_Call_ID = ''160916-2106''  AND Task_Line_Number = ''1''  ORDER BY DEX_ROW_ID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM HS_Tache WITH(NOLOCK) WHERE HS_Tache.Service_Call_ID = ''160916-2106''  AND HS_Tache.Task_Code = ''FA0023''  AND HS_Tache.Task_Line_Number = ''1'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT MAX(Task_Line_Number) FROM HS_Tache WHERE Service_Call_ID = ''160916-2106''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT FName + '' '' + LName FROM GMP_USAGERS WHERE UsagerID = ''NB50642V'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT FName + '' '' + LName FROM GMP_USAGERS WHERE UsagerID = ''NB50642V'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM HS_Tache  WHERE Service_Call_ID = ''160916-2106''  AND Task_Code = ''U0004''  AND Task_Line_Number = ''6'' ',@p3 output,@p4 output,@p5 output
"INSERT INTO HS_Tache (         CUSTNMBR         , ADRSCODE         , Service_Call_ID         , Equipment_ID         , Task_Code         , Task_Description         , Task_Line_Number         , SeqNumbr         , Estimate_Hours         , Time_Zone         , Completion_Date         , Task_Status          , Task_Answer         , Notes         , Partenaire         , CreateurUser         , CreateurDate         , ModifUser         , ModifDate         , DateMinimale         , DateMaximale         , Completion_Time         , DateAjoutBlaster  )  VALUES ('CL00471579' ,'SERVICE' ,'160916-2106' ,'' ,'U0004' ,'DELEGUE A' ,'6' ,'6' ,0 ,'1' ,CONVERT(DATETIME,'2016-11-10') ,'COMPLETE' ,'nb50642V' ,'Compte transf
 Nawel Benazza V par ed50617 via le gestionnaire de t
ches, pour toute information ou si un client appel veuillez communiquer avec Nawel Benazza V.' ,'nb50642V' ,'ed50617' ,CONVERT(DATETIME,'2016-11-10 13:41:45') ,'ed50617' ,CONVERT(DATETIME,'2016-11-10 13:41:45') ,CONVERT(DATETIME,'2016-11-10') ,CONVERT(DATETIME,'2016-12-10') ,CONVERT(DATETIME,'2016-11-10 13:41:45') ,CONVERT(DATETIME,'1900-01-01') )"
"INSERT INTO HS_Tache (         CUSTNMBR         , ADRSCODE         , Service_Call_ID         , Equipment_ID         , Task_Code         , Task_Description         , Task_Line_Number         , SeqNumbr         , Estimate_Hours         , Time_Zone         , Completion_Date         , Task_Status          , Task_Answer         , Notes         , Partenaire         , CreateurUser         , CreateurDate         , ModifUser         , ModifDate         , DateMinimale         , DateMaximale         , Completion_Time         , DateAjoutBlaster  )  VALUES ('CL00471579' ,'SERVICE' ,'160916-2106' ,'' ,'U0004' ,'DELEGUE A' ,'6' ,'6' ,0 ,'1' ,CONVERT(DATETIME,'2016-11-10') ,'COMPLETE' ,'nb50642V' ,'Compte transf
 Nawel Benazza V par ed50617 via le gestionnaire de t
ches, pour toute information ou si un client appel veuillez communiquer avec Nawel Benazza V.' ,'nb50642V' ,'ed50617' ,CONVERT(DATETIME,'2016-11-10 13:41:45') ,'ed50617' ,CONVERT(DATETIME,'2016-11-10 13:41:45') ,CONVERT(DATETIME,'2016-11-10') ,CONVERT(DATETIME,'2016-12-10') ,CONVERT(DATETIME,'2016-11-10 13:41:45') ,CONVERT(DATETIME,'1900-01-01') )"
exec sp_cursoropen @p1 output,N'select top 1 JoursAncienDelegue from gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM SV000805  WHERE CUSTNMBR = ''CL00471579''  AND ADRSCODE = ''SERVICE''  AND Reference_ID = ''160916-2106'' AND WS_Note_Type = ''S'' AND Note_Service_Index = ''6'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT *, IsNull(GMP_POSTAL.VenteSeul, 0) as VenteSeulement, IsNull(GMP_POSTAL.PasService, 1) as PasService, IsNull(GMP_POSTAL.SiSPA, 0) as SiSPA FROM HS_Client WITH(NOLOCK) LEFT JOIN GMP_POSTAL ON GMP_POSTAL.Postal = left(REPLACE(HS_Client.ZIP, '' '', ''''), (select max(LEN(Postal)) from GMP_POSTAL Where Postal = LEFT(REPLACE(HS_Client.ZIP, '' '', ''''), LEN(gmp_Postal.Postal)))) WHERE HS_Client.CUSTNMBR = ''CL00471579''  AND HS_Client.ADRSCODE = ''SERVICE'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'  SELECT (AGPERAMT_2+AGPERAMT_3) as Tot3089, (AGPERAMT_4+AGPERAMT_5+AGPERAMT_6+AGPERAMT_7) as Tot90Plus  FROM HYSEC..RM00103 WHERE CUSTNMBR=''CR00497237''     AND CUSTBLNC > 0 ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' select CUSTNMBR from HS_CHANGEMENT_ADRESSE  where CUSTNMBR =''CL00471579''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT FName + '' '' + LName FROM GMP_USAGERS WHERE UsagerID = ''ED50617'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM sv000805  WHERE CustNmbr = ''CL00471579''   AND AdrsCode = ''SERVICE''   AND Service_Call_ID = ''160916-2106''   AND WS_Note_Type = ''S''   AND Note_Service_Index = ''6''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM sv000805  WHERE CustNmbr = ''CL00471579''   AND AdrsCode = ''SERVICE''   AND Reference_ID = ''160916-2106''   AND WS_Note_Type = ''S''   AND Note_Service_Index = ''6''',@p3 output,@p4 output,@p5 output
"INSERT INTO sv000805 (        CustNmbr, AdrsCode, Reference_ID, Service_Call_ID       , Ws_Note_Type, Note_Service_Index, Note_Author       , Reminder, Reminder_Date, Reminder_Finish, Date1       , Time1, CustName, UserName, MdfUsrID, Record_Notes,Reminder_Author ) VALUES(   'CL00471579'   , 'SERVICE'   , '160916-2106'   , '160916-2106'   , 'S'   , '6'   , 'ed50617'   , '0'   , CONVERT(DATETIME,'1900-01-01 00:00:00')   , CONVERT(DATETIME,'1900-01-01')   , CONVERT(DATETIME,'2016-11-10')   , '13:41:46'   , 'PHARAND GILLES'   , 'Evelyne Djuikom'   , 'ed50617'   , 'Compte transf
 Nawel Benazza V par ed50617 via le gestionnaire de t
ches, pour toute information ou si un client appel veuillez communiquer avec Nawel Benazza V.'   , '')"
"INSERT INTO sv000805 (        CustNmbr, AdrsCode, Reference_ID, Service_Call_ID       , Ws_Note_Type, Note_Service_Index, Note_Author       , Reminder, Reminder_Date, Reminder_Finish, Date1       , Time1, CustName, UserName, MdfUsrID, Record_Notes,Reminder_Author ) VALUES(   'CL00471579'   , 'SERVICE'   , '160916-2106'   , '160916-2106'   , 'S'   , '6'   , 'ed50617'   , '0'   , CONVERT(DATETIME,'1900-01-01 00:00:00')   , CONVERT(DATETIME,'1900-01-01')   , CONVERT(DATETIME,'2016-11-10')   , '13:41:46'   , 'PHARAND GILLES'   , 'Evelyne Djuikom'   , 'ed50617'   , 'Compte transf
 Nawel Benazza V par ed50617 via le gestionnaire de t
ches, pour toute information ou si un client appel veuillez communiquer avec Nawel Benazza V.'   , '')"
exec sp_cursoropen @p1 output,N'SELECT SUM(Estimate_Hours) FROM HS_Tache WHERE CUSTNMBR = ''CL00471579'' AND Service_Call_ID = ''160916-2106''',@p3 output,@p4 output,@p5 output
UPDATE SV00301 SET Estimate_Hours = '0' WHERE Service_Call_ID = '160916-2106'
UPDATE HS_Appel SET Call_Time = '0' WHERE Service_Call_ID = '160916-2106'S_LISTE_TACHE  WHERE Service_Call_ID ='160916-2106' and Task_Code = 'U0004'
update HS_Appel set UsagerResponsable= 'nb50642V' where Service_Call_ID= '160916-2106      '
DELETE FROM HS_LISTE_TACHE  WHERE Service_Call_ID ='160916-2106'
exec sp_cursoropen @p1 output,N'select * from HS_LISTE_TACHE  WHERE Service_Call_ID =''160916-2106'' and Task_Code = ''FA0015     ''',@p3 output,@p4 output,@p5 output
"INSERT INTO hs_Liste_Tache   SELECT DISTINCT HS_Tache.DateMaximale AS DateMaximale         , HS_Tache.DateMinimale AS DateMinimale         , HS_Tache.Task_Description         , ISNULL(HS_Client.Service_Area,'') AS Service_Area         , ISNULL(HS_Client.User_Define_5a,'') AS User_Define_5a         , HS_Appel.CustNmbr AS CustNmbr         , HS_Appel.CustName AS CustName         , HS_Appel.AdrsCode AS AdrsCode         , HS_Appel.Call_Time         , HS_Appel.Service_Call_ID AS Service_Call_ID         , ISNULL(HS_Appel.UsagerResponsable,'') AS UsagerResponsable         , ISNULL(HS_Client.Service_User_Define_1,'') AS District         , HS_Tache.Task_Code         , HS_Tache.Task_Status         , HS_Appel.Type_of_Problem AS Type_of_Problem         , HS_Appel.Status_of_Call         , HS_Appel.Service_Description         , HS_Tache.Task_Line_Number AS SEQNUMBR         , HS_Appel.DateRdv         , HS_Appel.WSReserved_CB5         , HS_Client.Address1 AS Adresse         , ISNULL(HS_Produit.Contract_Number,'')as Contract_Number          , HS_Appel.Equipment_Type         , HS_Client.Address3         , HS_Client.User_Define_6a         , HS_Appel.Equipment_ID          , HS_Client.RegroupementID         , HS_Client.RegroupementUsagerID          , sv00560.Priority as Priority         , ''  FROM HS_Appel      LEFT JOIN HS_Tache ON HS_Tache.Service_Call_ID = HS_Appel.Service_Call_ID     LEFT JOIN HS_Client ON HS_Client.CustNmbr = HS_Appel.CustNmbr          AND HS_Client.AdrsCode = HS_Appel.AdrsCode     LEFT JOIN HS_Produit ON HS_Appel.Contract_Number = HS_Produit.Contract_Number     LEFT JOIN sv00560 on sv00560.Task_Code= HS_Tache.Task_Code  WHERE HS_Appel.Service_Call_ID ='160916-2106'     AND HS_Tache.Task_Code = 'FA0015     '     AND HS_Tache.Task_Status<>'COMPLETE'"
"INSERT INTO hs_Liste_Tache   SELECT DISTINCT HS_Tache.DateMaximale AS DateMaximale         , HS_Tache.DateMinimale AS DateMinimale         , HS_Tache.Task_Description         , ISNULL(HS_Client.Service_Area,'') AS Service_Area         , ISNULL(HS_Client.User_Define_5a,'') AS User_Define_5a         , HS_Appel.CustNmbr AS CustNmbr         , HS_Appel.CustName AS CustName         , HS_Appel.AdrsCode AS AdrsCode         , HS_Appel.Call_Time         , HS_Appel.Service_Call_ID AS Service_Call_ID         , ISNULL(HS_Appel.UsagerResponsable,'') AS UsagerResponsable         , ISNULL(HS_Client.Service_User_Define_1,'') AS District         , HS_Tache.Task_Code         , HS_Tache.Task_Status         , HS_Appel.Type_of_Problem AS Type_of_Problem         , HS_Appel.Status_of_Call         , HS_Appel.Service_Description         , HS_Tache.Task_Line_Number AS SEQNUMBR         , HS_Appel.DateRdv         , HS_Appel.WSReserved_CB5         , HS_Client.Address1 AS Adresse         , ISNULL(HS_Produit.Contract_Number,'')as Contract_Number          , HS_Appel.Equipment_Type         , HS_Client.Address3         , HS_Client.User_Define_6a         , HS_Appel.Equipment_ID          , HS_Client.RegroupementID         , HS_Client.RegroupementUsagerID          , sv00560.Priority as Priority         , ''  FROM HS_Appel      LEFT JOIN HS_Tache ON HS_Tache.Service_Call_ID = HS_Appel.Service_Call_ID     LEFT JOIN HS_Client ON HS_Client.CustNmbr = HS_Appel.CustNmbr          AND HS_Client.AdrsCode = HS_Appel.AdrsCode     LEFT JOIN HS_Produit ON HS_Appel.Contract_Number = HS_Produit.Contract_Number     LEFT JOIN sv00560 on sv00560.Task_Code= HS_Tache.Task_Code  WHERE HS_Appel.Service_Call_ID ='160916-2106'     AND HS_Tache.Task_Code = 'FA0015     '     AND HS_Tache.Task_Status<>'COMPLETE'"
exec sp_cursoropen @p1 output,N'SELECT *, IsNull(GMP_POSTAL.VenteSeul, 0) as VenteSeulement, IsNull(GMP_POSTAL.PasService, 1) as PasService, IsNull(GMP_POSTAL.SiSPA, 0) as SiSPA FROM HS_Client WITH(NOLOCK) LEFT JOIN GMP_POSTAL ON GMP_POSTAL.Postal = left(REPLACE(HS_Client.ZIP, '' '', ''''), (select max(LEN(Postal)) from GMP_POSTAL Where Postal = LEFT(REPLACE(HS_Client.ZIP, '' '', ''''), LEN(gmp_Postal.Postal)))) WHERE HS_Client.CUSTNMBR = ''CL00034720     ''  AND HS_Client.ADRSCODE = ''SERVICE        '' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'  SELECT (AGPERAMT_2+AGPERAMT_3) as Tot3089, (AGPERAMT_4+AGPERAMT_5+AGPERAMT_6+AGPERAMT_7) as Tot90Plus  FROM HYSEC..RM00103 WHERE CUSTNMBR=''CR01674097''     AND CUSTBLNC > 0 ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' select CUSTNMBR from HS_CHANGEMENT_ADRESSE  where CUSTNMBR =''CL00034720''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT HS_Appel.*, HS_Appel.*, HS_Appel.Service_Call_ID as Service_Call_ID, isnull(sv00310.Problem_ID, '''') as Problem_ID FROM HS_Appel WITH(NOLOCK) LEFT JOIN SV00310 WITH(NOLOCK) ON sv00310.Type_of_Problem= HS_Appel.Type_of_Problem WHERE HS_Appel.Service_Call_ID = ''160928-1634      '' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Task_Code FROM HS_Tache  WHERE Service_Call_ID = ''160928-1634''  AND Task_Line_Number = ''1''  ORDER BY DEX_ROW_ID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM HS_Tache WITH(NOLOCK) WHERE HS_Tache.Service_Call_ID = ''160928-1634''  AND HS_Tache.Task_Code = ''FA0023''  AND HS_Tache.Task_Line_Number = ''1'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT MAX(Task_Line_Number) FROM HS_Tache WHERE Service_Call_ID = ''160928-1634''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT FName + '' '' + LName FROM GMP_USAGERS WHERE UsagerID = ''NB50642V'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT FName + '' '' + LName FROM GMP_USAGERS WHERE UsagerID = ''NB50642V'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM HS_Tache  WHERE Service_Call_ID = ''160928-1634''  AND Task_Code = ''U0004''  AND Task_Line_Number = ''5'' ',@p3 output,@p4 output,@p5 output
"INSERT INTO HS_Tache (         CUSTNMBR         , ADRSCODE         , Service_Call_ID         , Equipment_ID         , Task_Code         , Task_Description         , Task_Line_Number         , SeqNumbr         , Estimate_Hours         , Time_Zone         , Completion_Date         , Task_Status          , Task_Answer         , Notes         , Partenaire         , CreateurUser         , CreateurDate         , ModifUser         , ModifDate         , DateMinimale         , DateMaximale         , Completion_Time         , DateAjoutBlaster  )  VALUES ('CL00034720' ,'SERVICE' ,'160928-1634' ,'' ,'U0004' ,'DELEGUE A' ,'5' ,'5' ,0 ,'1' ,CONVERT(DATETIME,'2016-11-10') ,'COMPLETE' ,'nb50642V' ,'Compte transf
 Nawel Benazza V par ed50617 via le gestionnaire de t
ches, pour toute information ou si un client appel veuillez communiquer avec Nawel Benazza V.' ,'nb50642V' ,'ed50617' ,CONVERT(DATETIME,'2016-11-10 13:41:46') ,'ed50617' ,CONVERT(DATETIME,'2016-11-10 13:41:46') ,CONVERT(DATETIME,'2016-11-10') ,CONVERT(DATETIME,'2016-12-10') ,CONVERT(DATETIME,'2016-11-10 13:41:46') ,CONVERT(DATETIME,'1900-01-01') )"
"INSERT INTO HS_Tache (         CUSTNMBR         , ADRSCODE         , Service_Call_ID         , Equipment_ID         , Task_Code         , Task_Description         , Task_Line_Number         , SeqNumbr         , Estimate_Hours         , Time_Zone         , Completion_Date         , Task_Status          , Task_Answer         , Notes         , Partenaire         , CreateurUser         , CreateurDate         , ModifUser         , ModifDate         , DateMinimale         , DateMaximale         , Completion_Time         , DateAjoutBlaster  )  VALUES ('CL00034720' ,'SERVICE' ,'160928-1634' ,'' ,'U0004' ,'DELEGUE A' ,'5' ,'5' ,0 ,'1' ,CONVERT(DATETIME,'2016-11-10') ,'COMPLETE' ,'nb50642V' ,'Compte transf
 Nawel Benazza V par ed50617 via le gestionnaire de t
ches, pour toute information ou si un client appel veuillez communiquer avec Nawel Benazza V.' ,'nb50642V' ,'ed50617' ,CONVERT(DATETIME,'2016-11-10 13:41:46') ,'ed50617' ,CONVERT(DATETIME,'2016-11-10 13:41:46') ,CONVERT(DATETIME,'2016-11-10') ,CONVERT(DATETIME,'2016-12-10') ,CONVERT(DATETIME,'2016-11-10 13:41:46') ,CONVERT(DATETIME,'1900-01-01') )"
exec sp_cursoropen @p1 output,N'select top 1 JoursAncienDelegue from gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM SV000805  WHERE CUSTNMBR = ''CL00034720''  AND ADRSCODE = ''SERVICE''  AND Reference_ID = ''160928-1634'' AND WS_Note_Type = ''S'' AND Note_Service_Index = ''5'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT *, IsNull(GMP_POSTAL.VenteSeul, 0) as VenteSeulement, IsNull(GMP_POSTAL.PasService, 1) as PasService, IsNull(GMP_POSTAL.SiSPA, 0) as SiSPA FROM HS_Client WITH(NOLOCK) LEFT JOIN GMP_POSTAL ON GMP_POSTAL.Postal = left(REPLACE(HS_Client.ZIP, '' '', ''''), (select max(LEN(Postal)) from GMP_POSTAL Where Postal = LEFT(REPLACE(HS_Client.ZIP, '' '', ''''), LEN(gmp_Postal.Postal)))) WHERE HS_Client.CUSTNMBR = ''CL00034720''  AND HS_Client.ADRSCODE = ''SERVICE'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'  SELECT (AGPERAMT_2+AGPERAMT_3) as Tot3089, (AGPERAMT_4+AGPERAMT_5+AGPERAMT_6+AGPERAMT_7) as Tot90Plus  FROM HYSEC..RM00103 WHERE CUSTNMBR=''CR01674097''     AND CUSTBLNC > 0 ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' select CUSTNMBR from HS_CHANGEMENT_ADRESSE  where CUSTNMBR =''CL00034720''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT FName + '' '' + LName FROM GMP_USAGERS WHERE UsagerID = ''ED50617'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM sv000805  WHERE CustNmbr = ''CL00034720''   AND AdrsCode = ''SERVICE''   AND Service_Call_ID = ''160928-1634''   AND WS_Note_Type = ''S''   AND Note_Service_Index = ''5''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM sv000805  WHERE CustNmbr = ''CL00034720''   AND AdrsCode = ''SERVICE''   AND Reference_ID = ''160928-1634''   AND WS_Note_Type = ''S''   AND Note_Service_Index = ''5''',@p3 output,@p4 output,@p5 output
"INSERT INTO sv000805 (        CustNmbr, AdrsCode, Reference_ID, Service_Call_ID       , Ws_Note_Type, Note_Service_Index, Note_Author       , Reminder, Reminder_Date, Reminder_Finish, Date1       , Time1, CustName, UserName, MdfUsrID, Record_Notes,Reminder_Author ) VALUES(   'CL00034720'   , 'SERVICE'   , '160928-1634'   , '160928-1634'   , 'S'   , '5'   , 'ed50617'   , '0'   , CONVERT(DATETIME,'1900-01-01 00:00:00')   , CONVERT(DATETIME,'1900-01-01')   , CONVERT(DATETIME,'2016-11-10')   , '13:41:47'   , 'NOUNKE CHARLOTTE'   , 'Evelyne Djuikom'   , 'ed50617'   , 'Compte transf
 Nawel Benazza V par ed50617 via le gestionnaire de t
ches, pour toute information ou si un client appel veuillez communiquer avec Nawel Benazza V.'   , '')"
"INSERT INTO sv000805 (        CustNmbr, AdrsCode, Reference_ID, Service_Call_ID       , Ws_Note_Type, Note_Service_Index, Note_Author       , Reminder, Reminder_Date, Reminder_Finish, Date1       , Time1, CustName, UserName, MdfUsrID, Record_Notes,Reminder_Author ) VALUES(   'CL00034720'   , 'SERVICE'   , '160928-1634'   , '160928-1634'   , 'S'   , '5'   , 'ed50617'   , '0'   , CONVERT(DATETIME,'1900-01-01 00:00:00')   , CONVERT(DATETIME,'1900-01-01')   , CONVERT(DATETIME,'2016-11-10')   , '13:41:47'   , 'NOUNKE CHARLOTTE'   , 'Evelyne Djuikom'   , 'ed50617'   , 'Compte transf
 Nawel Benazza V par ed50617 via le gestionnaire de t
ches, pour toute information ou si un client appel veuillez communiquer avec Nawel Benazza V.'   , '')"
exec sp_cursoropen @p1 output,N'SELECT SUM(Estimate_Hours) FROM HS_Tache WHERE CUSTNMBR = ''CL00034720'' AND Service_Call_ID = ''160928-1634''',@p3 output,@p4 output,@p5 output
UPDATE SV00301 SET Estimate_Hours = '0' WHERE Service_Call_ID = '160928-1634'
UPDATE SV00301 SET Estimate_Hours = '0' WHERE Service_Call_ID = '160928-1634'
UPDATE HS_Appel SET Call_Time = '0' WHERE Service_Call_ID = '160928-1634'
UPDATE HS_Appel SET Call_Time = '0' WHERE Service_Call_ID = '160928-1634'
delete from HS_LISTE_TACHE  WHERE Service_Call_ID ='160928-1634' and Task_Code = 'U0004'
delete from HS_LISTE_TACHE  WHERE Service_Call_ID ='160928-1634' and Task_Code = 'U0004'
update HS_Appel set UsagerResponsable= 'nb50642V' where Service_Call_ID= '160928-1634      '
update HS_Appel set UsagerResponsable= 'nb50642V' where Service_Call_ID= '160928-1634      '
DELETE FROM HS_LISTE_TACHE  WHERE Service_Call_ID ='160928-1634'
DELETE FROM HS_LISTE_TACHE  WHERE Service_Call_ID ='160928-1634'
exec sp_cursoropen @p1 output,N'select * from HS_LISTE_TACHE  WHERE Service_Call_ID =''160928-1634'' and Task_Code = ''FA0012     ''',@p3 output,@p4 output,@p5 output
"INSERT INTO hs_Liste_Tache   SELECT DISTINCT HS_Tache.DateMaximale AS DateMaximale         , HS_Tache.DateMinimale AS DateMinimale         , HS_Tache.Task_Description         , ISNULL(HS_Client.Service_Area,'') AS Service_Area         , ISNULL(HS_Client.User_Define_5a,'') AS User_Define_5a         , HS_Appel.CustNmbr AS CustNmbr         , HS_Appel.CustName AS CustName         , HS_Appel.AdrsCode AS AdrsCode         , HS_Appel.Call_Time         , HS_Appel.Service_Call_ID AS Service_Call_ID         , ISNULL(HS_Appel.UsagerResponsable,'') AS UsagerResponsable         , ISNULL(HS_Client.Service_User_Define_1,'') AS District         , HS_Tache.Task_Code         , HS_Tache.Task_Status         , HS_Appel.Type_of_Problem AS Type_of_Problem         , HS_Appel.Status_of_Call         , HS_Appel.Service_Description         , HS_Tache.Task_Line_Number AS SEQNUMBR         , HS_Appel.DateRdv         , HS_Appel.WSReserved_CB5         , HS_Client.Address1 AS Adresse         , ISNULL(HS_Produit.Contract_Number,'')as Contract_Number          , HS_Appel.Equipment_Type         , HS_Client.Address3         , HS_Client.User_Define_6a         , HS_Appel.Equipment_ID          , HS_Client.RegroupementID         , HS_Client.RegroupementUsagerID          , sv00560.Priority as Priority         , ''  FROM HS_Appel      LEFT JOIN HS_Tache ON HS_Tache.Service_Call_ID = HS_Appel.Service_Call_ID     LEFT JOIN HS_Client ON HS_Client.CustNmbr = HS_Appel.CustNmbr          AND HS_Client.AdrsCode = HS_Appel.AdrsCode     LEFT JOIN HS_Produit ON HS_Appel.Contract_Number = HS_Produit.Contract_Number     LEFT JOIN sv00560 on sv00560.Task_Code= HS_Tache.Task_Code  WHERE HS_Appel.Service_Call_ID ='160928-1634'     AND HS_Tache.Task_Code = 'FA0012     '     AND HS_Tache.Task_Status<>'COMPLETE'"
"INSERT INTO hs_Liste_Tache   SELECT DISTINCT HS_Tache.DateMaximale AS DateMaximale         , HS_Tache.DateMinimale AS DateMinimale         , HS_Tache.Task_Description         , ISNULL(HS_Client.Service_Area,'') AS Service_Area         , ISNULL(HS_Client.User_Define_5a,'') AS User_Define_5a         , HS_Appel.CustNmbr AS CustNmbr         , HS_Appel.CustName AS CustName         , HS_Appel.AdrsCode AS AdrsCode         , HS_Appel.Call_Time         , HS_Appel.Service_Call_ID AS Service_Call_ID         , ISNULL(HS_Appel.UsagerResponsable,'') AS UsagerResponsable         , ISNULL(HS_Client.Service_User_Define_1,'') AS District         , HS_Tache.Task_Code         , HS_Tache.Task_Status         , HS_Appel.Type_of_Problem AS Type_of_Problem         , HS_Appel.Status_of_Call         , HS_Appel.Service_Description         , HS_Tache.Task_Line_Number AS SEQNUMBR         , HS_Appel.DateRdv         , HS_Appel.WSReserved_CB5         , HS_Client.Address1 AS Adresse         , ISNULL(HS_Produit.Contract_Number,'')as Contract_Number          , HS_Appel.Equipment_Type         , HS_Client.Address3         , HS_Client.User_Define_6a         , HS_Appel.Equipment_ID          , HS_Client.RegroupementID         , HS_Client.RegroupementUsagerID          , sv00560.Priority as Priority         , ''  FROM HS_Appel      LEFT JOIN HS_Tache ON HS_Tache.Service_Call_ID = HS_Appel.Service_Call_ID     LEFT JOIN HS_Client ON HS_Client.CustNmbr = HS_Appel.CustNmbr          AND HS_Client.AdrsCode = HS_Appel.AdrsCode     LEFT JOIN HS_Produit ON HS_Appel.Contract_Number = HS_Produit.Contract_Number     LEFT JOIN sv00560 on sv00560.Task_Code= HS_Tache.Task_Code  WHERE HS_Appel.Service_Call_ID ='160928-1634'     AND HS_Tache.Task_Code = 'FA0012     '     AND HS_Tache.Task_Status<>'COMPLETE'"
exec sp_cursoropen @p1 output,N'SELECT *, IsNull(GMP_POSTAL.VenteSeul, 0) as VenteSeulement, IsNull(GMP_POSTAL.PasService, 1) as PasService, IsNull(GMP_POSTAL.SiSPA, 0) as SiSPA FROM HS_Client WITH(NOLOCK) LEFT JOIN GMP_POSTAL ON GMP_POSTAL.Postal = left(REPLACE(HS_Client.ZIP, '' '', ''''), (select max(LEN(Postal)) from GMP_POSTAL Where Postal = LEFT(REPLACE(HS_Client.ZIP, '' '', ''''), LEN(gmp_Postal.Postal)))) WHERE HS_Client.CUSTNMBR = ''CL00780522     ''  AND HS_Client.ADRSCODE = ''SERVICE        '' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'  SELECT (AGPERAMT_2+AGPERAMT_3) as Tot3089, (AGPERAMT_4+AGPERAMT_5+AGPERAMT_6+AGPERAMT_7) as Tot90Plus  FROM HYSEC..RM00103 WHERE CUSTNMBR=''CR01673807''     AND CUSTBLNC > 0 ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' select CUSTNMBR from HS_CHANGEMENT_ADRESSE  where CUSTNMBR =''CL00780522''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT HS_Appel.*, HS_Appel.*, HS_Appel.Service_Call_ID as Service_Call_ID, isnull(sv00310.Problem_ID, '''') as Problem_ID FROM HS_Appel WITH(NOLOCK) LEFT JOIN SV00310 WITH(NOLOCK) ON sv00310.Type_of_Problem= HS_Appel.Type_of_Problem WHERE HS_Appel.Service_Call_ID = ''160927-0780      '' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Task_Code FROM HS_Tache  WHERE Service_Call_ID = ''160927-0780''  AND Task_Line_Number = ''1''  ORDER BY DEX_ROW_ID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM HS_Tache WITH(NOLOCK) WHERE HS_Tache.Service_Call_ID = ''160927-0780''  AND HS_Tache.Task_Code = ''FA0023''  AND HS_Tache.Task_Line_Number = ''1'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT MAX(Task_Line_Number) FROM HS_Tache WHERE Service_Call_ID = ''160927-0780''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT FName + '' '' + LName FROM GMP_USAGERS WHERE UsagerID = ''NB50642V'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT FName + '' '' + LName FROM GMP_USAGERS WHERE UsagerID = ''NB50642V'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM HS_Tache  WHERE Service_Call_ID = ''160927-0780''  AND Task_Code = ''U0004''  AND Task_Line_Number = ''5'' ',@p3 output,@p4 output,@p5 output
"INSERT INTO HS_Tache (         CUSTNMBR         , ADRSCODE         , Service_Call_ID         , Equipment_ID         , Task_Code         , Task_Description         , Task_Line_Number         , SeqNumbr         , Estimate_Hours         , Time_Zone         , Completion_Date         , Task_Status          , Task_Answer         , Notes         , Partenaire         , CreateurUser         , CreateurDate         , ModifUser         , ModifDate         , DateMinimale         , DateMaximale         , Completion_Time         , DateAjoutBlaster  )  VALUES ('CL00780522' ,'SERVICE' ,'160927-0780' ,'' ,'U0004' ,'DELEGUE A' ,'5' ,'5' ,0 ,'1' ,CONVERT(DATETIME,'2016-11-10') ,'COMPLETE' ,'nb50642V' ,'Compte transf
 Nawel Benazza V par ed50617 via le gestionnaire de t
ches, pour toute information ou si un client appel veuillez communiquer avec Nawel Benazza V.' ,'nb50642V' ,'ed50617' ,CONVERT(DATETIME,'2016-11-10 13:41:48') ,'ed50617' ,CONVERT(DATETIME,'2016-11-10 13:41:48') ,CONVERT(DATETIME,'2016-11-10') ,CONVERT(DATETIME,'2016-12-10') ,CONVERT(DATETIME,'2016-11-10 13:41:48') ,CONVERT(DATETIME,'1900-01-01') )"
"INSERT INTO HS_Tache (         CUSTNMBR         , ADRSCODE         , Service_Call_ID         , Equipment_ID         , Task_Code         , Task_Description         , Task_Line_Number         , SeqNumbr         , Estimate_Hours         , Time_Zone         , Completion_Date         , Task_Status          , Task_Answer         , Notes         , Partenaire         , CreateurUser         , CreateurDate         , ModifUser         , ModifDate         , DateMinimale         , DateMaximale         , Completion_Time         , DateAjoutBlaster  )  VALUES ('CL00780522' ,'SERVICE' ,'160927-0780' ,'' ,'U0004' ,'DELEGUE A' ,'5' ,'5' ,0 ,'1' ,CONVERT(DATETIME,'2016-11-10') ,'COMPLETE' ,'nb50642V' ,'Compte transf
 Nawel Benazza V par ed50617 via le gestionnaire de t
ches, pour toute information ou si un client appel veuillez communiquer avec Nawel Benazza V.' ,'nb50642V' ,'ed50617' ,CONVERT(DATETIME,'2016-11-10 13:41:48') ,'ed50617' ,CONVERT(DATETIME,'2016-11-10 13:41:48') ,CONVERT(DATETIME,'2016-11-10') ,CONVERT(DATETIME,'2016-12-10') ,CONVERT(DATETIME,'2016-11-10 13:41:48') ,CONVERT(DATETIME,'1900-01-01') )"
exec sp_cursoropen @p1 output,N'select top 1 JoursAncienDelegue from gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM SV000805  WHERE CUSTNMBR = ''CL00780522''  AND ADRSCODE = ''SERVICE''  AND Reference_ID = ''160927-0780'' AND WS_Note_Type = ''S'' AND Note_Service_Index = ''5'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT *, IsNull(GMP_POSTAL.VenteSeul, 0) as VenteSeulement, IsNull(GMP_POSTAL.PasService, 1) as PasService, IsNull(GMP_POSTAL.SiSPA, 0) as SiSPA FROM HS_Client WITH(NOLOCK) LEFT JOIN GMP_POSTAL ON GMP_POSTAL.Postal = left(REPLACE(HS_Client.ZIP, '' '', ''''), (select max(LEN(Postal)) from GMP_POSTAL Where Postal = LEFT(REPLACE(HS_Client.ZIP, '' '', ''''), LEN(gmp_Postal.Postal)))) WHERE HS_Client.CUSTNMBR = ''CL00780522''  AND HS_Client.ADRSCODE = ''SERVICE'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'  SELECT (AGPERAMT_2+AGPERAMT_3) as Tot3089, (AGPERAMT_4+AGPERAMT_5+AGPERAMT_6+AGPERAMT_7) as Tot90Plus  FROM HYSEC..RM00103 WHERE CUSTNMBR=''CR01673807''     AND CUSTBLNC > 0 ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' select CUSTNMBR from HS_CHANGEMENT_ADRESSE  where CUSTNMBR =''CL00780522''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT FName + '' '' + LName FROM GMP_USAGERS WHERE UsagerID = ''ED50617'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM sv000805  WHERE CustNmbr = ''CL00780522''   AND AdrsCode = ''SERVICE''   AND Service_Call_ID = ''160927-0780''   AND WS_Note_Type = ''S''   AND Note_Service_Index = ''5''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM sv000805  WHERE CustNmbr = ''CL00780522''   AND AdrsCode = ''SERVICE''   AND Reference_ID = ''160927-0780''   AND WS_Note_Type = ''S''   AND Note_Service_Index = ''5''',@p3 output,@p4 output,@p5 output
"INSERT INTO sv000805 (        CustNmbr, AdrsCode, Reference_ID, Service_Call_ID       , Ws_Note_Type, Note_Service_Index, Note_Author       , Reminder, Reminder_Date, Reminder_Finish, Date1       , Time1, CustName, UserName, MdfUsrID, Record_Notes,Reminder_Author ) VALUES(   'CL00780522'   , 'SERVICE'   , '160927-0780'   , '160927-0780'   , 'S'   , '5'   , 'ed50617'   , '0'   , CONVERT(DATETIME,'1900-01-01 00:00:00')   , CONVERT(DATETIME,'1900-01-01')   , CONVERT(DATETIME,'2016-11-10')   , '13:41:48'   , 'SUCCESSION GAGNE RONALD'   , 'Evelyne Djuikom'   , 'ed50617'   , 'Compte transf
 Nawel Benazza V par ed50617 via le gestionnaire de t
ches, pour toute information ou si un client appel veuillez communiquer avec Nawel Benazza V.'   , '')"
"INSERT INTO sv000805 (        CustNmbr, AdrsCode, Reference_ID, Service_Call_ID       , Ws_Note_Type, Note_Service_Index, Note_Author       , Reminder, Reminder_Date, Reminder_Finish, Date1       , Time1, CustName, UserName, MdfUsrID, Record_Notes,Reminder_Author ) VALUES(   'CL00780522'   , 'SERVICE'   , '160927-0780'   , '160927-0780'   , 'S'   , '5'   , 'ed50617'   , '0'   , CONVERT(DATETIME,'1900-01-01 00:00:00')   , CONVERT(DATETIME,'1900-01-01')   , CONVERT(DATETIME,'2016-11-10')   , '13:41:48'   , 'SUCCESSION GAGNE RONALD'   , 'Evelyne Djuikom'   , 'ed50617'   , 'Compte transf
 Nawel Benazza V par ed50617 via le gestionnaire de t
ches, pour toute information ou si un client appel veuillez communiquer avec Nawel Benazza V.'   , '')"
exec sp_cursoropen @p1 output,N'SELECT SUM(Estimate_Hours) FROM HS_Tache WHERE CUSTNMBR = ''CL00780522'' AND Service_Call_ID = ''160927-0780''',@p3 output,@p4 output,@p5 output
UPDATE SV00301 SET Estimate_Hours = '0' WHERE Service_Call_ID = '160927-0780'
UPDATE SV00301 SET Estimate_Hours = '0' WHERE Service_Call_ID = '160927-0780'
UPDATE HS_Appel SET Call_Time = '0' WHERE Service_Call_ID = '160927-0780'
UPDATE HS_Appel SET Call_Time = '0' WHERE Service_Call_ID = '160927-0780'
delete from HS_LISTE_TACHE  WHERE Service_Call_ID ='160927-0780' and Task_Code = 'U0004'
delete from HS_LISTE_TACHE  WHERE Service_Call_ID ='160927-0780' and Task_Code = 'U0004'
update HS_Appel set UsagerResponsable= 'nb50642V' where Service_Call_ID= '160927-0780      '
update HS_Appel set UsagerResponsable= 'nb50642V' where Service_Call_ID= '160927-0780      '
DELETE FROM HS_LISTE_TACHE  WHERE Service_Call_ID ='160927-0780'
DELETE FROM HS_LISTE_TACHE  WHERE Service_Call_ID ='160927-0780'
exec sp_cursoropen @p1 output,N'select * from HS_LISTE_TACHE  WHERE Service_Call_ID =''160927-0780'' and Task_Code = ''FA0023     ''',@p3 output,@p4 output,@p5 output
"INSERT INTO hs_Liste_Tache   SELECT DISTINCT HS_Tache.DateMaximale AS DateMaximale         , HS_Tache.DateMinimale AS DateMinimale         , HS_Tache.Task_Description         , ISNULL(HS_Client.Service_Area,'') AS Service_Area         , ISNULL(HS_Client.User_Define_5a,'') AS User_Define_5a         , HS_Appel.CustNmbr AS CustNmbr         , HS_Appel.CustName AS CustName         , HS_Appel.AdrsCode AS AdrsCode         , HS_Appel.Call_Time         , HS_Appel.Service_Call_ID AS Service_Call_ID         , ISNULL(HS_Appel.UsagerResponsable,'') AS UsagerResponsable         , ISNULL(HS_Client.Service_User_Define_1,'') AS District         , HS_Tache.Task_Code         , HS_Tache.Task_Status         , HS_Appel.Type_of_Problem AS Type_of_Problem         , HS_Appel.Status_of_Call         , HS_Appel.Service_Description         , HS_Tache.Task_Line_Number AS SEQNUMBR         , HS_Appel.DateRdv         , HS_Appel.WSReserved_CB5         , HS_Client.Address1 AS Adresse         , ISNULL(HS_Produit.Contract_Number,'')as Contract_Number          , HS_Appel.Equipment_Type         , HS_Client.Address3         , HS_Client.User_Define_6a         , HS_Appel.Equipment_ID          , HS_Client.RegroupementID         , HS_Client.RegroupementUsagerID          , sv00560.Priority as Priority         , ''  FROM HS_Appel      LEFT JOIN HS_Tache ON HS_Tache.Service_Call_ID = HS_Appel.Service_Call_ID     LEFT JOIN HS_Client ON HS_Client.CustNmbr = HS_Appel.CustNmbr          AND HS_Client.AdrsCode = HS_Appel.AdrsCode     LEFT JOIN HS_Produit ON HS_Appel.Contract_Number = HS_Produit.Contract_Number     LEFT JOIN sv00560 on sv00560.Task_Code= HS_Tache.Task_Code  WHERE HS_Appel.Service_Call_ID ='160927-0780'     AND HS_Tache.Task_Code = 'FA0023     '     AND HS_Tache.Task_Status<>'COMPLETE'"
"INSERT INTO hs_Liste_Tache   SELECT DISTINCT HS_Tache.DateMaximale AS DateMaximale         , HS_Tache.DateMinimale AS DateMinimale         , HS_Tache.Task_Description         , ISNULL(HS_Client.Service_Area,'') AS Service_Area         , ISNULL(HS_Client.User_Define_5a,'') AS User_Define_5a         , HS_Appel.CustNmbr AS CustNmbr         , HS_Appel.CustName AS CustName         , HS_Appel.AdrsCode AS AdrsCode         , HS_Appel.Call_Time         , HS_Appel.Service_Call_ID AS Service_Call_ID         , ISNULL(HS_Appel.UsagerResponsable,'') AS UsagerResponsable         , ISNULL(HS_Client.Service_User_Define_1,'') AS District         , HS_Tache.Task_Code         , HS_Tache.Task_Status         , HS_Appel.Type_of_Problem AS Type_of_Problem         , HS_Appel.Status_of_Call         , HS_Appel.Service_Description         , HS_Tache.Task_Line_Number AS SEQNUMBR         , HS_Appel.DateRdv         , HS_Appel.WSReserved_CB5         , HS_Client.Address1 AS Adresse         , ISNULL(HS_Produit.Contract_Number,'')as Contract_Number          , HS_Appel.Equipment_Type         , HS_Client.Address3         , HS_Client.User_Define_6a         , HS_Appel.Equipment_ID          , HS_Client.RegroupementID         , HS_Client.RegroupementUsagerID          , sv00560.Priority as Priority         , ''  FROM HS_Appel      LEFT JOIN HS_Tache ON HS_Tache.Service_Call_ID = HS_Appel.Service_Call_ID     LEFT JOIN HS_Client ON HS_Client.CustNmbr = HS_Appel.CustNmbr          AND HS_Client.AdrsCode = HS_Appel.AdrsCode     LEFT JOIN HS_Produit ON HS_Appel.Contract_Number = HS_Produit.Contract_Number     LEFT JOIN sv00560 on sv00560.Task_Code= HS_Tache.Task_Code  WHERE HS_Appel.Service_Call_ID ='160927-0780'     AND HS_Tache.Task_Code = 'FA0023     '     AND HS_Tache.Task_Status<>'COMPLETE'"
exec sp_cursoropen @p1 output,N'SELECT *, IsNull(GMP_POSTAL.VenteSeul, 0) as VenteSeulement, IsNull(GMP_POSTAL.PasService, 1) as PasService, IsNull(GMP_POSTAL.SiSPA, 0) as SiSPA FROM HS_Client WITH(NOLOCK) LEFT JOIN GMP_POSTAL ON GMP_POSTAL.Postal = left(REPLACE(HS_Client.ZIP, '' '', ''''), (select max(LEN(Postal)) from GMP_POSTAL Where Postal = LEFT(REPLACE(HS_Client.ZIP, '' '', ''''), LEN(gmp_Postal.Postal)))) WHERE HS_Client.CUSTNMBR = ''CL00152815     ''  AND HS_Client.ADRSCODE = ''SERVICE        '' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'  SELECT (AGPERAMT_2+AGPERAMT_3) as Tot3089, (AGPERAMT_4+AGPERAMT_5+AGPERAMT_6+AGPERAMT_7) as Tot90Plus  FROM HYSEC..RM00103 WHERE CUSTNMBR=''CR01371167''     AND CUSTBLNC > 0 ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' select CUSTNMBR from HS_CHANGEMENT_ADRESSE  where CUSTNMBR =''CL00152815''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT HS_Appel.*, HS_Appel.*, HS_Appel.Service_Call_ID as Service_Call_ID, isnull(sv00310.Problem_ID, '''') as Problem_ID FROM HS_Appel WITH(NOLOCK) LEFT JOIN SV00310 WITH(NOLOCK) ON sv00310.Type_of_Problem= HS_Appel.Type_of_Problem WHERE HS_Appel.Service_Call_ID = ''160924-0831      '' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Task_Code FROM HS_Tache  WHERE Service_Call_ID = ''160924-0831''  AND Task_Line_Number = ''1''  ORDER BY DEX_ROW_ID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM HS_Tache WITH(NOLOCK) WHERE HS_Tache.Service_Call_ID = ''160924-0831''  AND HS_Tache.Task_Code = ''FA0018''  AND HS_Tache.Task_Line_Number = ''1'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT MAX(Task_Line_Number) FROM HS_Tache WHERE Service_Call_ID = ''160924-0831''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT FName + '' '' + LName FROM GMP_USAGERS WHERE UsagerID = ''NB50642V'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT FName + '' '' + LName FROM GMP_USAGERS WHERE UsagerID = ''NB50642V'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM HS_Tache  WHERE Service_Call_ID = ''160924-0831''  AND Task_Code = ''U0004''  AND Task_Line_Number = ''5'' ',@p3 output,@p4 output,@p5 output
"INSERT INTO HS_Tache (         CUSTNMBR         , ADRSCODE         , Service_Call_ID         , Equipment_ID         , Task_Code         , Task_Description         , Task_Line_Number         , SeqNumbr         , Estimate_Hours         , Time_Zone         , Completion_Date         , Task_Status          , Task_Answer         , Notes         , Partenaire         , CreateurUser         , CreateurDate         , ModifUser         , ModifDate         , DateMinimale         , DateMaximale         , Completion_Time         , DateAjoutBlaster  )  VALUES ('CL00152815' ,'SERVICE' ,'160924-0831' ,'' ,'U0004' ,'DELEGUE A' ,'5' ,'5' ,0 ,'1' ,CONVERT(DATETIME,'2016-11-10') ,'COMPLETE' ,'nb50642V' ,'Compte transf
 Nawel Benazza V par ed50617 via le gestionnaire de t
ches, pour toute information ou si un client appel veuillez communiquer avec Nawel Benazza V.' ,'nb50642V' ,'ed50617' ,CONVERT(DATETIME,'2016-11-10 13:41:49') ,'ed50617' ,CONVERT(DATETIME,'2016-11-10 13:41:49') ,CONVERT(DATETIME,'2016-11-10') ,CONVERT(DATETIME,'2016-12-10') ,CONVERT(DATETIME,'2016-11-10 13:41:49') ,CONVERT(DATETIME,'1900-01-01') )"
"INSERT INTO HS_Tache (         CUSTNMBR         , ADRSCODE         , Service_Call_ID         , Equipment_ID         , Task_Code         , Task_Description         , Task_Line_Number         , SeqNumbr         , Estimate_Hours         , Time_Zone         , Completion_Date         , Task_Status          , Task_Answer         , Notes         , Partenaire         , CreateurUser         , CreateurDate         , ModifUser         , ModifDate         , DateMinimale         , DateMaximale         , Completion_Time         , DateAjoutBlaster  )  VALUES ('CL00152815' ,'SERVICE' ,'160924-0831' ,'' ,'U0004' ,'DELEGUE A' ,'5' ,'5' ,0 ,'1' ,CONVERT(DATETIME,'2016-11-10') ,'COMPLETE' ,'nb50642V' ,'Compte transf
 Nawel Benazza V par ed50617 via le gestionnaire de t
ches, pour toute information ou si un client appel veuillez communiquer avec Nawel Benazza V.' ,'nb50642V' ,'ed50617' ,CONVERT(DATETIME,'2016-11-10 13:41:49') ,'ed50617' ,CONVERT(DATETIME,'2016-11-10 13:41:49') ,CONVERT(DATETIME,'2016-11-10') ,CONVERT(DATETIME,'2016-12-10') ,CONVERT(DATETIME,'2016-11-10 13:41:49') ,CONVERT(DATETIME,'1900-01-01') )"
exec sp_cursoropen @p1 output,N'select top 1 JoursAncienDelegue from gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM SV000805  WHERE CUSTNMBR = ''CL00152815''  AND ADRSCODE = ''SERVICE''  AND Reference_ID = ''160924-0831'' AND WS_Note_Type = ''S'' AND Note_Service_Index = ''5'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT *, IsNull(GMP_POSTAL.VenteSeul, 0) as VenteSeulement, IsNull(GMP_POSTAL.PasService, 1) as PasService, IsNull(GMP_POSTAL.SiSPA, 0) as SiSPA FROM HS_Client WITH(NOLOCK) LEFT JOIN GMP_POSTAL ON GMP_POSTAL.Postal = left(REPLACE(HS_Client.ZIP, '' '', ''''), (select max(LEN(Postal)) from GMP_POSTAL Where Postal = LEFT(REPLACE(HS_Client.ZIP, '' '', ''''), LEN(gmp_Postal.Postal)))) WHERE HS_Client.CUSTNMBR = ''CL00152815''  AND HS_Client.ADRSCODE = ''SERVICE'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'  SELECT (AGPERAMT_2+AGPERAMT_3) as Tot3089, (AGPERAMT_4+AGPERAMT_5+AGPERAMT_6+AGPERAMT_7) as Tot90Plus  FROM HYSEC..RM00103 WHERE CUSTNMBR=''CR01371167''     AND CUSTBLNC > 0 ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' select CUSTNMBR from HS_CHANGEMENT_ADRESSE  where CUSTNMBR =''CL00152815''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT FName + '' '' + LName FROM GMP_USAGERS WHERE UsagerID = ''ED50617'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM sv000805  WHERE CustNmbr = ''CL00152815''   AND AdrsCode = ''SERVICE''   AND Service_Call_ID = ''160924-0831''   AND WS_Note_Type = ''S''   AND Note_Service_Index = ''5''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM sv000805  WHERE CustNmbr = ''CL00152815''   AND AdrsCode = ''SERVICE''   AND Reference_ID = ''160924-0831''   AND WS_Note_Type = ''S''   AND Note_Service_Index = ''5''',@p3 output,@p4 output,@p5 output
"INSERT INTO sv000805 (        CustNmbr, AdrsCode, Reference_ID, Service_Call_ID       , Ws_Note_Type, Note_Service_Index, Note_Author       , Reminder, Reminder_Date, Reminder_Finish, Date1       , Time1, CustName, UserName, MdfUsrID, Record_Notes,Reminder_Author ) VALUES(   'CL00152815'   , 'SERVICE'   , '160924-0831'   , '160924-0831'   , 'S'   , '5'   , 'ed50617'   , '0'   , CONVERT(DATETIME,'1900-01-01 00:00:00')   , CONVERT(DATETIME,'1900-01-01')   , CONVERT(DATETIME,'2016-11-10')   , '13:41:49'   , 'SERGE SAUVAGEAU'   , 'Evelyne Djuikom'   , 'ed50617'   , 'Compte transf
 Nawel Benazza V par ed50617 via le gestionnaire de t
ches, pour toute information ou si un client appel veuillez communiquer avec Nawel Benazza V.'   , '')"
"INSERT INTO sv000805 (        CustNmbr, AdrsCode, Reference_ID, Service_Call_ID       , Ws_Note_Type, Note_Service_Index, Note_Author       , Reminder, Reminder_Date, Reminder_Finish, Date1       , Time1, CustName, UserName, MdfUsrID, Record_Notes,Reminder_Author ) VALUES(   'CL00152815'   , 'SERVICE'   , '160924-0831'   , '160924-0831'   , 'S'   , '5'   , 'ed50617'   , '0'   , CONVERT(DATETIME,'1900-01-01 00:00:00')   , CONVERT(DATETIME,'1900-01-01')   , CONVERT(DATETIME,'2016-11-10')   , '13:41:49'   , 'SERGE SAUVAGEAU'   , 'Evelyne Djuikom'   , 'ed50617'   , 'Compte transf
 Nawel Benazza V par ed50617 via le gestionnaire de t
ches, pour toute information ou si un client appel veuillez communiquer avec Nawel Benazza V.'   , '')"
exec sp_cursoropen @p1 output,N'SELECT SUM(Estimate_Hours) FROM HS_Tache WHERE CUSTNMBR = ''CL00152815'' AND Service_Call_ID = ''160924-0831''',@p3 output,@p4 output,@p5 output
UPDATE SV00301 SET Estimate_Hours = '0' WHERE Service_Call_ID = '160924-0831'
UPDATE HS_Appel SET Call_Time = '0' WHERE Service_Call_ID = '160924-0831'
delete from HS_LISTE_TACHE  WHERE Service_Call_ID ='160924-0831' and Task_Code = 'U0004'
update HS_Appel set UsagerResponsable= 'nb50642V' where Service_Call_ID= '160924-0831      '
DELETE FROM HS_LISTE_TACHE  WHERE Service_Call_ID ='160924-0831'
exec sp_cursoropen @p1 output,N'select * from HS_LISTE_TACHE  WHERE Service_Call_ID =''160924-0831'' and Task_Code = ''FA0018     ''',@p3 output,@p4 output,@p5 output
"INSERT INTO hs_Liste_Tache   SELECT DISTINCT HS_Tache.DateMaximale AS DateMaximale         , HS_Tache.DateMinimale AS DateMinimale         , HS_Tache.Task_Description         , ISNULL(HS_Client.Service_Area,'') AS Service_Area         , ISNULL(HS_Client.User_Define_5a,'') AS User_Define_5a         , HS_Appel.CustNmbr AS CustNmbr         , HS_Appel.CustName AS CustName         , HS_Appel.AdrsCode AS AdrsCode         , HS_Appel.Call_Time         , HS_Appel.Service_Call_ID AS Service_Call_ID         , ISNULL(HS_Appel.UsagerResponsable,'') AS UsagerResponsable         , ISNULL(HS_Client.Service_User_Define_1,'') AS District         , HS_Tache.Task_Code         , HS_Tache.Task_Status         , HS_Appel.Type_of_Problem AS Type_of_Problem         , HS_Appel.Status_of_Call         , HS_Appel.Service_Description         , HS_Tache.Task_Line_Number AS SEQNUMBR         , HS_Appel.DateRdv         , HS_Appel.WSReserved_CB5         , HS_Client.Address1 AS Adresse         , ISNULL(HS_Produit.Contract_Number,'')as Contract_Number          , HS_Appel.Equipment_Type         , HS_Client.Address3         , HS_Client.User_Define_6a         , HS_Appel.Equipment_ID          , HS_Client.RegroupementID         , HS_Client.RegroupementUsagerID          , sv00560.Priority as Priority         , ''  FROM HS_Appel      LEFT JOIN HS_Tache ON HS_Tache.Service_Call_ID = HS_Appel.Service_Call_ID     LEFT JOIN HS_Client ON HS_Client.CustNmbr = HS_Appel.CustNmbr          AND HS_Client.AdrsCode = HS_Appel.AdrsCode     LEFT JOIN HS_Produit ON HS_Appel.Contract_Number = HS_Produit.Contract_Number     LEFT JOIN sv00560 on sv00560.Task_Code= HS_Tache.Task_Code  WHERE HS_Appel.Service_Call_ID ='160924-0831'     AND HS_Tache.Task_Code = 'FA0018     '     AND HS_Tache.Task_Status<>'COMPLETE'"
"INSERT INTO hs_Liste_Tache   SELECT DISTINCT HS_Tache.DateMaximale AS DateMaximale         , HS_Tache.DateMinimale AS DateMinimale         , HS_Tache.Task_Description         , ISNULL(HS_Client.Service_Area,'') AS Service_Area         , ISNULL(HS_Client.User_Define_5a,'') AS User_Define_5a         , HS_Appel.CustNmbr AS CustNmbr         , HS_Appel.CustName AS CustName         , HS_Appel.AdrsCode AS AdrsCode         , HS_Appel.Call_Time         , HS_Appel.Service_Call_ID AS Service_Call_ID         , ISNULL(HS_Appel.UsagerResponsable,'') AS UsagerResponsable         , ISNULL(HS_Client.Service_User_Define_1,'') AS District         , HS_Tache.Task_Code         , HS_Tache.Task_Status         , HS_Appel.Type_of_Problem AS Type_of_Problem         , HS_Appel.Status_of_Call         , HS_Appel.Service_Description         , HS_Tache.Task_Line_Number AS SEQNUMBR         , HS_Appel.DateRdv         , HS_Appel.WSReserved_CB5         , HS_Client.Address1 AS Adresse         , ISNULL(HS_Produit.Contract_Number,'')as Contract_Number          , HS_Appel.Equipment_Type         , HS_Client.Address3         , HS_Client.User_Define_6a         , HS_Appel.Equipment_ID          , HS_Client.RegroupementID         , HS_Client.RegroupementUsagerID          , sv00560.Priority as Priority         , ''  FROM HS_Appel      LEFT JOIN HS_Tache ON HS_Tache.Service_Call_ID = HS_Appel.Service_Call_ID     LEFT JOIN HS_Client ON HS_Client.CustNmbr = HS_Appel.CustNmbr          AND HS_Client.AdrsCode = HS_Appel.AdrsCode     LEFT JOIN HS_Produit ON HS_Appel.Contract_Number = HS_Produit.Contract_Number     LEFT JOIN sv00560 on sv00560.Task_Code= HS_Tache.Task_Code  WHERE HS_Appel.Service_Call_ID ='160924-0831'     AND HS_Tache.Task_Code = 'FA0018     '     AND HS_Tache.Task_Status<>'COMPLETE'"
exec sp_cursoropen @p1 output,N'SELECT *, IsNull(GMP_POSTAL.VenteSeul, 0) as VenteSeulement, IsNull(GMP_POSTAL.PasService, 1) as PasService, IsNull(GMP_POSTAL.SiSPA, 0) as SiSPA FROM HS_Client WITH(NOLOCK) LEFT JOIN GMP_POSTAL ON GMP_POSTAL.Postal = left(REPLACE(HS_Client.ZIP, '' '', ''''), (select max(LEN(Postal)) from GMP_POSTAL Where Postal = LEFT(REPLACE(HS_Client.ZIP, '' '', ''''), LEN(gmp_Postal.Postal)))) WHERE HS_Client.CUSTNMBR = ''CL00181582     ''  AND HS_Client.ADRSCODE = ''SERVICE        '' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'  SELECT (AGPERAMT_2+AGPERAMT_3) as Tot3089, (AGPERAMT_4+AGPERAMT_5+AGPERAMT_6+AGPERAMT_7) as Tot90Plus  FROM HYSEC..RM00103 WHERE CUSTNMBR=''CR01508264''     AND CUSTBLNC > 0 ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' select CUSTNMBR from HS_CHANGEMENT_ADRESSE  where CUSTNMBR =''CL00181582''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT HS_Appel.*, HS_Appel.*, HS_Appel.Service_Call_ID as Service_Call_ID, isnull(sv00310.Problem_ID, '''') as Problem_ID FROM HS_Appel WITH(NOLOCK) LEFT JOIN SV00310 WITH(NOLOCK) ON sv00310.Type_of_Problem= HS_Appel.Type_of_Problem WHERE HS_Appel.Service_Call_ID = ''161010-0110      '' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Task_Code FROM HS_Tache  WHERE Service_Call_ID = ''161010-0110''  AND Task_Line_Number = ''1''  ORDER BY DEX_ROW_ID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM HS_Tache WITH(NOLOCK) WHERE HS_Tache.Service_Call_ID = ''161010-0110''  AND HS_Tache.Task_Code = ''FA0023''  AND HS_Tache.Task_Line_Number = ''1'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT MAX(Task_Line_Number) FROM HS_Tache WHERE Service_Call_ID = ''161010-0110''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT FName + '' '' + LName FROM GMP_USAGERS WHERE UsagerID = ''NB50642V'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT FName + '' '' + LName FROM GMP_USAGERS WHERE UsagerID = ''NB50642V'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM HS_Tache  WHERE Service_Call_ID = ''161010-0110''  AND Task_Code = ''U0004''  AND Task_Line_Number = ''5'' ',@p3 output,@p4 output,@p5 output
"INSERT INTO HS_Tache (         CUSTNMBR         , ADRSCODE         , Service_Call_ID         , Equipment_ID         , Task_Code         , Task_Description         , Task_Line_Number         , SeqNumbr         , Estimate_Hours         , Time_Zone         , Completion_Date         , Task_Status          , Task_Answer         , Notes         , Partenaire         , CreateurUser         , CreateurDate         , ModifUser         , ModifDate         , DateMinimale         , DateMaximale         , Completion_Time         , DateAjoutBlaster  )  VALUES ('CL00181582' ,'SERVICE' ,'161010-0110' ,'' ,'U0004' ,'DELEGUE A' ,'5' ,'5' ,0 ,'1' ,CONVERT(DATETIME,'2016-11-10') ,'COMPLETE' ,'nb50642V' ,'Compte transf
 Nawel Benazza V par ed50617 via le gestionnaire de t
ches, pour toute information ou si un client appel veuillez communiquer avec Nawel Benazza V.' ,'nb50642V' ,'ed50617' ,CONVERT(DATETIME,'2016-11-10 13:41:50') ,'ed50617' ,CONVERT(DATETIME,'2016-11-10 13:41:50') ,CONVERT(DATETIME,'2016-11-10') ,CONVERT(DATETIME,'2016-12-10') ,CONVERT(DATETIME,'2016-11-10 13:41:50') ,CONVERT(DATETIME,'1900-01-01') )"
"INSERT INTO HS_Tache (         CUSTNMBR         , ADRSCODE         , Service_Call_ID         , Equipment_ID         , Task_Code         , Task_Description         , Task_Line_Number         , SeqNumbr         , Estimate_Hours         , Time_Zone         , Completion_Date         , Task_Status          , Task_Answer         , Notes         , Partenaire         , CreateurUser         , CreateurDate         , ModifUser         , ModifDate         , DateMinimale         , DateMaximale         , Completion_Time         , DateAjoutBlaster  )  VALUES ('CL00181582' ,'SERVICE' ,'161010-0110' ,'' ,'U0004' ,'DELEGUE A' ,'5' ,'5' ,0 ,'1' ,CONVERT(DATETIME,'2016-11-10') ,'COMPLETE' ,'nb50642V' ,'Compte transf
 Nawel Benazza V par ed50617 via le gestionnaire de t
ches, pour toute information ou si un client appel veuillez communiquer avec Nawel Benazza V.' ,'nb50642V' ,'ed50617' ,CONVERT(DATETIME,'2016-11-10 13:41:50') ,'ed50617' ,CONVERT(DATETIME,'2016-11-10 13:41:50') ,CONVERT(DATETIME,'2016-11-10') ,CONVERT(DATETIME,'2016-12-10') ,CONVERT(DATETIME,'2016-11-10 13:41:50') ,CONVERT(DATETIME,'1900-01-01') )"
exec sp_cursoropen @p1 output,N'select top 1 JoursAncienDelegue from gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM SV000805  WHERE CUSTNMBR = ''CL00181582''  AND ADRSCODE = ''SERVICE''  AND Reference_ID = ''161010-0110'' AND WS_Note_Type = ''S'' AND Note_Service_Index = ''5'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT *, IsNull(GMP_POSTAL.VenteSeul, 0) as VenteSeulement, IsNull(GMP_POSTAL.PasService, 1) as PasService, IsNull(GMP_POSTAL.SiSPA, 0) as SiSPA FROM HS_Client WITH(NOLOCK) LEFT JOIN GMP_POSTAL ON GMP_POSTAL.Postal = left(REPLACE(HS_Client.ZIP, '' '', ''''), (select max(LEN(Postal)) from GMP_POSTAL Where Postal = LEFT(REPLACE(HS_Client.ZIP, '' '', ''''), LEN(gmp_Postal.Postal)))) WHERE HS_Client.CUSTNMBR = ''CL00181582''  AND HS_Client.ADRSCODE = ''SERVICE'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'  SELECT (AGPERAMT_2+AGPERAMT_3) as Tot3089, (AGPERAMT_4+AGPERAMT_5+AGPERAMT_6+AGPERAMT_7) as Tot90Plus  FROM HYSEC..RM00103 WHERE CUSTNMBR=''CR01508264''     AND CUSTBLNC > 0 ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' select CUSTNMBR from HS_CHANGEMENT_ADRESSE  where CUSTNMBR =''CL00181582''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT FName + '' '' + LName FROM GMP_USAGERS WHERE UsagerID = ''ED50617'' ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM sv000805  WHERE CustNmbr = ''CL00181582''   AND AdrsCode = ''SERVICE''   AND Service_Call_ID = ''161010-0110''   AND WS_Note_Type = ''S''   AND Note_Service_Index = ''5''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM sv000805  WHERE CustNmbr = ''CL00181582''   AND AdrsCode = ''SERVICE''   AND Reference_ID = ''161010-0110''   AND WS_Note_Type = ''S''   AND Note_Service_Index = ''5''',@p3 output,@p4 output,@p5 output
"INSERT INTO sv000805 (        CustNmbr, AdrsCode, Reference_ID, Service_Call_ID       , Ws_Note_Type, Note_Service_Index, Note_Author       , Reminder, Reminder_Date, Reminder_Finish, Date1       , Time1, CustName, UserName, MdfUsrID, Record_Notes,Reminder_Author ) VALUES(   'CL00181582'   , 'SERVICE'   , '161010-0110'   , '161010-0110'   , 'S'   , '5'   , 'ed50617'   , '0'   , CONVERT(DATETIME,'1900-01-01 00:00:00')   , CONVERT(DATETIME,'1900-01-01')   , CONVERT(DATETIME,'2016-11-10')   , '13:41:50'   , 'PIERRETTE PAQUETTE POITRAS'   , 'Evelyne Djuikom'   , 'ed50617'   , 'Compte transf
 Nawel Benazza V par ed50617 via le gestionnaire de t
ches, pour toute information ou si un client appel veuillez communiquer avec Nawel Benazza V.'   , '')"
"INSERT INTO sv000805 (        CustNmbr, AdrsCode, Reference_ID, Service_Call_ID       , Ws_Note_Type, Note_Service_Index, Note_Author       , Reminder, Reminder_Date, Reminder_Finish, Date1       , Time1, CustName, UserName, MdfUsrID, Record_Notes,Reminder_Author ) VALUES(   'CL00181582'   , 'SERVICE'   , '161010-0110'   , '161010-0110'   , 'S'   , '5'   , 'ed50617'   , '0'   , CONVERT(DATETIME,'1900-01-01 00:00:00')   , CONVERT(DATETIME,'1900-01-01')   , CONVERT(DATETIME,'2016-11-10')   , '13:41:50'   , 'PIERRETTE PAQUETTE POITRAS'   , 'Evelyne Djuikom'   , 'ed50617'   , 'Compte transf
 Nawel Benazza V par ed50617 via le gestionnaire de t
ches, pour toute information ou si un client appel veuillez communiquer avec Nawel Benazza V.'   , '')"
exec sp_cursoropen @p1 output,N'SELECT SUM(Estimate_Hours) FROM HS_Tache WHERE CUSTNMBR = ''CL00181582'' AND Service_Call_ID = ''161010-0110''',@p3 output,@p4 output,@p5 output
UPDATE SV00301 SET Estimate_Hours = '0' WHERE Service_Call_ID = '161010-0110'
UPDATE SV00301 SET Estimate_Hours = '0' WHERE Service_Call_ID = '161010-0110'
UPDATE HS_Appel SET Call_Time = '0' WHERE Service_Call_ID = '161010-0110'
UPDATE HS_Appel SET Call_Time = '0' WHERE Service_Call_ID = '161010-0110'
delete from HS_LISTE_TACHE  WHERE Service_Call_ID ='161010-0110' and Task_Code = 'U0004'
delete from HS_LISTE_TACHE  WHERE Service_Call_ID ='161010-0110' and Task_Code = 'U0004'
update HS_Appel set UsagerResponsable= 'nb50642V' where Service_Call_ID= '161010-0110      '
update HS_Appel set UsagerResponsable= 'nb50642V' where Service_Call_ID= '161010-0110      '
DELETE FROM HS_LISTE_TACHE  WHERE Service_Call_ID ='161010-0110'
DELETE FROM HS_LISTE_TACHE  WHERE Service_Call_ID ='161010-0110'
exec sp_cursoropen @p1 output,N'select * from HS_LISTE_TACHE  WHERE Service_Call_ID =''161010-0110'' and Task_Code = ''FA0024     ''',@p3 output,@p4 output,@p5 output
"INSERT INTO hs_Liste_Tache   SELECT DISTINCT HS_Tache.DateMaximale AS DateMaximale         , HS_Tache.DateMinimale AS DateMinimale         , HS_Tache.Task_Description         , ISNULL(HS_Client.Service_Area,'') AS Service_Area         , ISNULL(HS_Client.User_Define_5a,'') AS User_Define_5a         , HS_Appel.CustNmbr AS CustNmbr         , HS_Appel.CustName AS CustName         , HS_Appel.AdrsCode AS AdrsCode         , HS_Appel.Call_Time         , HS_Appel.Service_Call_ID AS Service_Call_ID         , ISNULL(HS_Appel.UsagerResponsable,'') AS UsagerResponsable         , ISNULL(HS_Client.Service_User_Define_1,'') AS District         , HS_Tache.Task_Code         , HS_Tache.Task_Status         , HS_Appel.Type_of_Problem AS Type_of_Problem         , HS_Appel.Status_of_Call         , HS_Appel.Service_Description         , HS_Tache.Task_Line_Number AS SEQNUMBR         , HS_Appel.DateRdv         , HS_Appel.WSReserved_CB5         , HS_Client.Address1 AS Adresse         , ISNULL(HS_Produit.Contract_Number,'')as Contract_Number          , HS_Appel.Equipment_Type         , HS_Client.Address3         , HS_Client.User_Define_6a         , HS_Appel.Equipment_ID          , HS_Client.RegroupementID         , HS_Client.RegroupementUsagerID          , sv00560.Priority as Priority         , ''  FROM HS_Appel      LEFT JOIN HS_Tache ON HS_Tache.Service_Call_ID = HS_Appel.Service_Call_ID     LEFT JOIN HS_Client ON HS_Client.CustNmbr = HS_Appel.CustNmbr          AND HS_Client.AdrsCode = HS_Appel.AdrsCode     LEFT JOIN HS_Produit ON HS_Appel.Contract_Number = HS_Produit.Contract_Number     LEFT JOIN sv00560 on sv00560.Task_Code= HS_Tache.Task_Code  WHERE HS_Appel.Service_Call_ID ='161010-0110'     AND HS_Tache.Task_Code = 'FA0024     '     AND HS_Tache.Task_Status<>'COMPLETE'"
"INSERT INTO hs_Liste_Tache   SELECT DISTINCT HS_Tache.DateMaximale AS DateMaximale         , HS_Tache.DateMinimale AS DateMinimale         , HS_Tache.Task_Description         , ISNULL(HS_Client.Service_Area,'') AS Service_Area         , ISNULL(HS_Client.User_Define_5a,'') AS User_Define_5a         , HS_Appel.CustNmbr AS CustNmbr         , HS_Appel.CustName AS CustName         , HS_Appel.AdrsCode AS AdrsCode         , HS_Appel.Call_Time         , HS_Appel.Service_Call_ID AS Service_Call_ID         , ISNULL(HS_Appel.UsagerResponsable,'') AS UsagerResponsable         , ISNULL(HS_Client.Service_User_Define_1,'') AS District         , HS_Tache.Task_Code         , HS_Tache.Task_Status         , HS_Appel.Type_of_Problem AS Type_of_Problem         , HS_Appel.Status_of_Call         , HS_Appel.Service_Description         , HS_Tache.Task_Line_Number AS SEQNUMBR         , HS_Appel.DateRdv         , HS_Appel.WSReserved_CB5         , HS_Client.Address1 AS Adresse         , ISNULL(HS_Produit.Contract_Number,'')as Contract_Number          , HS_Appel.Equipment_Type         , HS_Client.Address3         , HS_Client.User_Define_6a         , HS_Appel.Equipment_ID          , HS_Client.RegroupementID         , HS_Client.RegroupementUsagerID          , sv00560.Priority as Priority         , ''  FROM HS_Appel      LEFT JOIN HS_Tache ON HS_Tache.Service_Call_ID = HS_Appel.Service_Call_ID     LEFT JOIN HS_Client ON HS_Client.CustNmbr = HS_Appel.CustNmbr          AND HS_Client.AdrsCode = HS_Appel.AdrsCode     LEFT JOIN HS_Produit ON HS_Appel.Contract_Number = HS_Produit.Contract_Number     LEFT JOIN sv00560 on sv00560.Task_Code= HS_Tache.Task_Code  WHERE HS_Appel.Service_Call_ID ='161010-0110'     AND HS_Tache.Task_Code = 'FA0024     '     AND HS_Tache.Task_Status<>'COMPLETE'"
exec sp_cursoropen @p1 output,N' SELECT UPPER(LName + '' '' + FName) + ''
('' + UsagerID + '')'' as UID FROM Gmp_Usagers WHERE UsagerID <> '''' AND UsagerID IN (SELECT DISTINCT UsagerResponsable FROM hs_Liste_tache) ORDER BY 1 ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT RTRIM(Type_of_Problem) Type_of_Problem, COUNT(*) as NbTachesOuvertes FROM HS_Liste_Tache hlt WHERE UsagerResponsable = '''' GROUP BY RTRIM(Type_of_Problem) ORDER BY 1',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ISNULL(Regroupement, ''[AUCUN]'') as Regroupement FROM HS_Liste_Tache hlt   LEFT JOIN hs_Regroupement r on hlt.RegroupementID= r.ID WHERE UsagerResponsable = '''' GROUP BY Regroupement ORDER BY 1',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT RTRIM(Task_Description) + ''
('' + RTRIM(Task_Code) + '')'' as Tache, RTRIM(Task_Code) as Task_Code, COUNT(*) as NbTachesOuvertes FROM HS_Liste_Tache hlt   LEFT JOIN hs_Regroupement r on hlt.Regroupementid= r.ID WHERE UsagerResponsable = '''' GROUP BY RTRIM(Task_Description) + ''
('' + RTRIM(Task_Code) + '')'', RTRIM(Task_Code) ORDER BY 1',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT DISTINCT ''[GROUPE] '' + Groupe as Groupe FROM hs_groupedelegation  ORDER BY 1',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT UPPER(LName + '' '' + FName) + ''
('' + UsagerID + '')'' as UID FROM Gmp_Usagers WHERE UsagerID <> ''''    AND Inactif= 0   AND UsagerID not in (''sa'', ''sadev'', ''scripths'') ORDER BY 1 ',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT DISTINCT 0 id, ''D
faut (aucun regroupement)'' regroupement UNION SELECT DISTINCT  id, regroupement FROM hs_Regroupement ORDER BY 1',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT u.Fname, u.LName, u.UsagerID FROM hs_regroupementUsager ru     INNER JOIN hs_regroupement r on ru.RegroupementID= r.ID     INNER JOIN gmp_Usagers u on u.UsagerID= ru.UsagerID         AND r.Regroupement= '''' ORDER BY 2,1,3',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT u.Fname, u.LName, u.UsagerID FROM gmp_usagers u  WHERE u.Inactif= 0   AND u.UsagerID NOT IN (           SELECT ru.UsagerID           FROM hs_Regroupement r                INNER JOIN hs_RegroupementUsager ru on r.ID = ru.RegroupementID                   AND r.Regroupement= ''''           )  AND u.UsagerID NOT IN (''sa'', ''AgentPartenaire'', ''SCRIPTHS'') ORDER BY 2,1,3',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT DISTINCT ''[GROUPE] '' + Groupe as groupe FROM hs_GroupeDelegation UNION SELECT DISTINCT ''[REGROUPEMENT] '' + Regroupement groupe FROM hs_Regroupement ORDER BY 1',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT u.Fname, u.LName, u.UsagerID FROM hs_GroupeDelegation gd   INNER JOIN gmp_usagers u on gd.UsagerID= u.UsagerID       AND gd.Groupe= ''APPEL COURTOISIE'' where RegroupementClient = 0 ORDER BY 2,1,3',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT u.Fname, u.LName, u.UsagerID FROM gmp_usagers u  WHERE u.Inactif= 0   AND u.UsagerID NOT IN (           SELECT gd.UsagerID           FROM hs_GroupeDelegation gd           WHERE gd.Groupe= ''APPEL COURTOISIE''           and RegroupementClient = 0           )  AND u.UsagerID NOT IN (''sa'', ''AgentPartenaire'', ''SCRIPTHS'')  ORDER BY 2,1,3',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT u.Fname, u.LName, u.UsagerID FROM hs_GroupeDelegation gd   INNER JOIN gmp_usagers u on gd.UsagerID= u.UsagerID       AND gd.Groupe= ''PERCEPTION'' where RegroupementClient = 0 ORDER BY 2,1,3',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT u.Fname, u.LName, u.UsagerID FROM gmp_usagers u  WHERE u.Inactif= 0   AND u.UsagerID NOT IN (           SELECT gd.UsagerID           FROM hs_GroupeDelegation gd           WHERE gd.Groupe= ''PERCEPTION''           and RegroupementClient = 0           )  AND u.UsagerID NOT IN (''sa'', ''AgentPartenaire'', ''SCRIPTHS'')  ORDER BY 2,1,3',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT @@TRANCOUNT',@p3 output,@p4 output,@p5 output
DELETE FROM hs_GroupeDelegation WHERE Groupe= 'PERCEPTION' and RegroupementClient = 0
INSERT INTO hs_GroupeDelegation (Groupe, UsagerID, RegroupementClient)    VALUES ('PERCEPTION', 'gakbwe', 0 )
INSERT INTO hs_GroupeDelegation (Groupe, UsagerID, RegroupementClient)    VALUES ('PERCEPTION', '8058', 0 )
INSERT INTO hs_GroupeDelegation (Groupe, UsagerID, RegroupementClient)    VALUES ('PERCEPTION', 'CE02', 0 )
INSERT INTO hs_GroupeDelegation (Groupe, UsagerID, RegroupementClient)    VALUES ('PERCEPTION', '6304', 0 )
exec sp_cursoropen @p1 output,N'SELECT @@TRANCOUNT',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT u.Fname, u.LName, u.UsagerID FROM hs_GroupeDelegation gd   INNER JOIN gmp_usagers u on gd.UsagerID= u.UsagerID       AND gd.Groupe= ''RETENTION'' where RegroupementClient = 0 ORDER BY 2,1,3',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT u.Fname, u.LName, u.UsagerID FROM gmp_usagers u  WHERE u.Inactif= 0   AND u.UsagerID NOT IN (           SELECT gd.UsagerID           FROM hs_GroupeDelegation gd           WHERE gd.Groupe= ''RETENTION''           and RegroupementClient = 0           )  AND u.UsagerID NOT IN (''sa'', ''AgentPartenaire'', ''SCRIPTHS'')  ORDER BY 2,1,3',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT @@TRANCOUNT',@p3 output,@p4 output,@p5 output
DELETE FROM hs_GroupeDelegation WHERE Groupe= 'RETENTION' and RegroupementClient = 0
INSERT INTO hs_GroupeDelegation (Groupe, UsagerID, RegroupementClient)    VALUES ('RETENTION', 'mf50542', 0 )
INSERT INTO hs_GroupeDelegation (Groupe, UsagerID, RegroupementClient)    VALUES ('RETENTION', 'ihamdiV', 0 )
exec sp_cursoropen @p1 output,N'SELECT @@TRANCOUNT',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT u.Fname, u.LName, u.UsagerID FROM hs_regroupementUsager ru     INNER JOIN hs_regroupement r on ru.RegroupementID= r.ID     INNER JOIN gmp_Usagers u on u.UsagerID= ru.UsagerID         AND r.Regroupement= ''*** Compte RELIANCE ***'' ORDER BY 2,1,3',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT u.Fname, u.LName, u.UsagerID FROM gmp_usagers u  WHERE u.Inactif= 0   AND u.UsagerID NOT IN (           SELECT ru.UsagerID           FROM hs_Regroupement r                INNER JOIN hs_RegroupementUsager ru on r.ID = ru.RegroupementID                   AND r.Regroupement= ''*** Compte RELIANCE ***''           )  AND u.UsagerID NOT IN (''sa'', ''AgentPartenaire'', ''SCRIPTHS'') ORDER BY 2,1,3',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT u.Fname, u.LName, u.UsagerID FROM hs_GroupeDelegation gd   INNER JOIN gmp_usagers u on gd.UsagerID= u.UsagerID       AND gd.Groupe= ''PERCEPTION'' where RegroupementClient = 0 ORDER BY 2,1,3',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT u.Fname, u.LName, u.UsagerID FROM gmp_usagers u  WHERE u.Inactif= 0   AND u.UsagerID NOT IN (           SELECT gd.UsagerID           FROM hs_GroupeDelegation gd           WHERE gd.Groupe= ''PERCEPTION''           and RegroupementClient = 0           )  AND u.UsagerID NOT IN (''sa'', ''AgentPartenaire'', ''SCRIPTHS'')  ORDER BY 2,1,3',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT u.Fname, u.LName, u.UsagerID FROM hs_GroupeDelegation gd   INNER JOIN gmp_usagers u on gd.UsagerID= u.UsagerID       AND gd.Groupe= ''PERCEPTION'' where RegroupementClient = 5 ORDER BY 2,1,3',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT u.Fname, u.LName, u.UsagerID FROM gmp_usagers u  WHERE u.Inactif= 0   AND u.UsagerID NOT IN (           SELECT gd.UsagerID           FROM hs_GroupeDelegation gd           WHERE gd.Groupe= ''PERCEPTION''           and RegroupementClient = 5           )  AND u.UsagerID NOT IN (''sa'', ''AgentPartenaire'', ''SCRIPTHS'')  ORDER BY 2,1,3',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT @@TRANCOUNT',@p3 output,@p4 output,@p5 output
DELETE FROM hs_GroupeDelegation WHERE Groupe= 'PERCEPTION' and RegroupementClient = 5
INSERT INTO hs_GroupeDelegation (Groupe, UsagerID, RegroupementClient)    VALUES ('PERCEPTION', 'mm50586', 5 )
INSERT INTO hs_GroupeDelegation (Groupe, UsagerID, RegroupementClient)    VALUES ('PERCEPTION', 'nchicoine', 5 )
exec sp_cursoropen @p1 output,N'SELECT @@TRANCOUNT',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' Select  * from GMP_PARTENAIRES  where Traitement=''1'' and ID_Partenaire <>''''  and Resolution_Code<>''''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'select rtrim(Type_of_problem) as Type_of_problem  from sv00310 order by Type_of_problem',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'select Task_code, Equipment_type + '' - '' + Task_code + '' - '' + task_description as description  from sv00560 order by description',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'select * from sv00310 inner join gmp_sv00310  on sv00310.type_of_problem = gmp_sv00310.type_of_problem where sv00310.type_of_problem = ''15% ASSUR SPA-BASE''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'select * from SV00560 where Equipment_Type = ''15% ASSUR SPA-BASE''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
"INSERT INTO GMP_LOG_SCRIPT (Date_Log, Objet, Description, Usager_ID, Number, Parametres, Source, VersionApp) VALUES (CONVERT(DATETIME,'2016-11-10 13:45:18'), 'frmVolumeBlaster.Form_Load', '[DBNETLIB][ConnectionOpen (Connect()).]SQL Server does not exist or access denied.', 'ed50617', '-2147467259', '', 'Microsoft OLE DB Provider for SQL Server', 'HydroSoft_5-96-3')"
"INSERT INTO GMP_LOG_SCRIPT (Date_Log, Objet, Description, Usager_ID, Number, Parametres, Source, VersionApp) VALUES (CONVERT(DATETIME,'2016-11-10 13:45:18'), 'frmVolumeBlaster.Form_Load', '[DBNETLIB][ConnectionOpen (Connect()).]SQL Server does not exist or access denied.', 'ed50617', '-2147467259', '', 'Microsoft OLE DB Provider for SQL Server', 'HydroSoft_5-96-3')"
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_LOG_SCRIPT_SENT WHERE Corrige = 0     AND VersionApp = ''HydroSoft_5-96-3''     AND DateDiff(day, DateLog,''2016-11-10 13:45:18'') <= 7     AND Objet = ''frmVolumeBlaster.Form_Load''     AND Number = ''-2147467259''     AND Source = ''Microsoft OLE DB Provider for SQL Server''     AND SUBSTRING(Description, 1, IIF(CHARINDEX('''''''', Description) = 0, LEN(Description), CHARINDEX('''''''', Description) - 1)) = ''[DBNETLIB][ConnectionOpen (Connect()).]SQL Server does not exist or access denied.''',@p3 output,@p4 output,@p5 output
"INSERT INTO GMP_LOG_SCRIPT_SENT (DateLog, Objet, Description, UsagerID, Number, Parametres, Source, VersionApp) VALUES (CONVERT(DATETIME,'2016-11-10 13:45:18'), 'frmVolumeBlaster.Form_Load', '[DBNETLIB][ConnectionOpen (Connect()).]SQL Server does not exist or access denied.', 'ed50617', '-2147467259', '', 'Microsoft OLE DB Provider for SQL Server', 'HydroSoft_5-96-3')"
"INSERT INTO GMP_LOG_SCRIPT_SENT (DateLog, Objet, Description, UsagerID, Number, Parametres, Source, VersionApp) VALUES (CONVERT(DATETIME,'2016-11-10 13:45:18'), 'frmVolumeBlaster.Form_Load', '[DBNETLIB][ConnectionOpen (Connect()).]SQL Server does not exist or access denied.', 'ed50617', '-2147467259', '', 'Microsoft OLE DB Provider for SQL Server', 'HydroSoft_5-96-3')"
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_USAGERS WITH(NOLOCK) WHERE UsagerID = ''ed50617'' ',@p3 output,@p4 output,@p5 output
"INSERT INTO GMP_COURRIELS (To_Address, From_Address, Subject, Body, Request_Date,Piece_Jointe, envoyer)  VALUES ('edjuikom@hydrosolution.com', 'Hydrosoft-Bug@HydroSolution.com', 'TEST - Erreur dans frmVolumeBlaster.Form_Load  -  [DBNETLIB][ConnectionOpen (Connect()).]SQL Serv...', 'Une erreur Hydrosoft est survenue.
=======================
- VERSION : 
	HydroSoft_5-96-3
- DATE DE L''ERREUR : 
	2016-11-10 13:45:18
- USAGER : 
	Evelyne Djuikom (ed50617) - Groupe de securite : ADMIN
- NUMERO DE L''ERREUR : 
	-2147467259
- DESCRIPTION : 
	[DBNETLIB][ConnectionOpen (Connect()).]SQL Server does not exist or access denied.
- OBJET : 
	frmVolumeBlaster.Form_Load
- SOURCE : 
	Microsoft OLE DB Provider for SQL Server
- PARAMETRES : 
	', CONVERT(DATETIME,'2016-11-10 13:45:19'), '', 0)"
"INSERT INTO GMP_COURRIELS (To_Address, From_Address, Subject, Body, Request_Date,Piece_Jointe, envoyer)  VALUES ('edjuikom@hydrosolution.com', 'Hydrosoft-Bug@HydroSolution.com', 'TEST - Erreur dans frmVolumeBlaster.Form_Load  -  [DBNETLIB][ConnectionOpen (Connect()).]SQL Serv...', 'Une erreur Hydrosoft est survenue.
=======================
- VERSION : 
	HydroSoft_5-96-3
- DATE DE L''ERREUR : 
	2016-11-10 13:45:18
- USAGER : 
	Evelyne Djuikom (ed50617) - Groupe de securite : ADMIN
- NUMERO DE L''ERREUR : 
	-2147467259
- DESCRIPTION : 
	[DBNETLIB][ConnectionOpen (Connect()).]SQL Server does not exist or access denied.
- OBJET : 
	frmVolumeBlaster.Form_Load
- SOURCE : 
	Microsoft OLE DB Provider for SQL Server
- PARAMETRES : 
	', CONVERT(DATETIME,'2016-11-10 13:45:19'), '', 0)"
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
INSERT INTO GMP_LOG_SCRIPT (Date_Log, Objet, Description, Usager_ID, Number, Parametres, Source, VersionApp) VALUES (CONVERT(DATETIME,'2016-11-10 13:45:47'), 'frmVolumeBlaster.Form_Load', '[DBNETLIB][ConnectionOpen (Connect()).]SQL Server does not exist or access denied.', 'ed50617', '-2147467259', '', 'Microsoft OLE DB Provider for SQL Server', 'HydroSoft_5-96-3')
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_LOG_SCRIPT_SENT WHERE Corrige = 0     AND VersionApp = ''HydroSoft_5-96-3''     AND DateDiff(day, DateLog,''2016-11-10 13:45:47'') <= 7     AND Objet = ''frmVolumeBlaster.Form_Load''     AND Number = ''-2147467259''     AND Source = ''Microsoft OLE DB Provider for SQL Server''     AND SUBSTRING(Description, 1, IIF(CHARINDEX('''''''', Description) = 0, LEN(Description), CHARINDEX('''''''', Description) - 1)) = ''[DBNETLIB][ConnectionOpen (Connect()).]SQL Server does not exist or access denied.''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_Rapport ORDER BY ParentID, isHeader desc, Name',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 1 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 110 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''50'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 114 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''51'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 118 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''52'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 3 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''41'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 2 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''26'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 107 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''50'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 97 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''41'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 46 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 68 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 45 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 44 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 43 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 120 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-26'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 49 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-26'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 51 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-26'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 47 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-26'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 81 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''26'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 54 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''26'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 48 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''26'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 89 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 101 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 85 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''26'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 91 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-26'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 65 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''26'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 64 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''26'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 53 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-26'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 52 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-26'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 88 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''41'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 86 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''41'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 58 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''41'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 100 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-41'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 59 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''41'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 60 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''41'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 67 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 13 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 29 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 27 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 75 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 33 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 23 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 24 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 76 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 74 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 35 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 26 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 11 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 36 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 17 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 93 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 37 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 87 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 84 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 15 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 9 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 39 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 19 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 21 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 20 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 96 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 10 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 22 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 92 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 4 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 18 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 83 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 78 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 122 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 79 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 102 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 41 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 32 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 77 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 95 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 94 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 66 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 82 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 30 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 73 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 62 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 63 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 34 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 28 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 40 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 80 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 61 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 106 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 121 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 25 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 69 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 71 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 70 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 90 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 72 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 98 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''41'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 105 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''41'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 103 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''41'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 99 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''-25'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 117 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''41'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 104 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''41'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 108 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''50'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 109 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''50'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 111 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''50'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 112 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''50'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 113 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''50'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 115 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''51'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 116 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''51'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT * FROM hs_RapportParam where RapportID= 119 ORDER BY ParamID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT Security_Group FROM GMP_USAGERS WHERE UsagerID = ''ed50617''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'SELECT * FROM GMP_SECURITEACCESS  WHERE ITEM_ID=''52'' AND USER_CLASS=''ADMIN''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'select distinct Type_Of_Problem from sv00310 order by 1',@p3 output,@p4 output,@p5 output
INSERT INTO [dbo].[HS_Log_ImpressionRapports]            ([DateDemande]            ,[Usager]            ,[FonctionRapport]            ,[Arguments])      Values            (GETDATE()            ,'ed50617'            ,'rptCall_TachesInOut'            ,'2015-11-10 \\ 2016-11-09 \\ 2016-11-10'            )
DROP TABLE workTacheInOut_ed50617
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
"SELECT *
 INTO workTacheInOut_ed50617
 FROM (
     SELECT A.CustNmbr, A.AdrsCode,  A.Service_CAll_ID, A.Type_of_Problem, A.Service_Description, A.Date_of_Service_Call
         , a.Status_of_Call, a.Completion_Date CompletionDateAppel, a.Urgence
         , a.Createur CreateurAppel, a.ModifyUser as ModifyUserAppel
         , a.ModifyDate as ModifyDateAppel, a.UsagerResponsable as UsagerResponsableAppel
         , a.CloserUser as CloserUserAppel, a.CloserDate as CloserDateAppel
         , sv00560.Priority as PriorityTache
         , sv00560.TypeUsagerTache
         , t.Task_Code, t.Task_Line_Number, t.Task_Description, t.Completion_Date CompletionDateTache, t.Task_Status
         , t.CreateurUser as CreateurTache, t.CreateurDate as CreateurDateTache
         , t.ModifUser as ModifyUserTache, t.ModifDate as ModifyDateTache
         , t.DateMinimale, t.DateMaximale
         , t.Task_Answer, t.Partenaire as PartenaireTache
         , t.Completion_Time as CompletionTimeTache
         , 'IN' as ModifierType
         , 1 as ModifierInOut
         , a.Resolution_Description
         , a.Transaction_Type
         , Notes = CASE
                      WHEN t.Notes <> '' THEN t.Notes
                      WHEN t.Notes = '' AND n.Record_Notes is not null THEN n.Record_Notes
                      ELSE ''
                  END
         , a.SiCODebranche
         , ISNULL(ra.Description, 'S/O') as RaisonAnnulation
     FROM HS_Appel a
        LEFT JOIN hs_raisonannulation ra on a.RaisonAnnulation= ra.ID
         LEFT JOIN (select * from sv000805 aa  where note_author <> 'SA' and  (date1 + time1) = (select max(date1 + time1) from sv000805 ab where aa.service_call_id = ab.service_call_id )) n on n.Service_Call_ID = a.Service_Call_ID
         INNER JOIN HS_Tache t on a.Service_Call_ID= t.Service_Call_ID
          INNER JOIN sv00560 on sv00560.Task_Code= t.Task_Code
     WHERE CreateurDate>= CONVERT(DATETIME,'2016-11-09')
         AND CreateurDate < CONVERT(DATETIME,'2016-11-10')
     UNION ALL
     SELECT A.CustNmbr, A.AdrsCode,  A.Service_CAll_ID, A.Type_of_Problem, A.Service_Description, A.Date_of_Service_Call
         , a.Status_of_Call, a.Completion_Date CompletionDateAppel, a.Urgence
         , a.Createur CreateurAppel, a.ModifyUser as ModifyUserAppel
         , a.ModifyDate as ModifyDateAppel, a.UsagerResponsable as UsagerResponsableAppel
         , a.CloserUser as CloserUserAppel, a.CloserDate as CloserDateAppel
         , sv00560.Priority as PriorityTache
         , sv00560.TypeUsagerTache
         , t.Task_Code, t.Task_Line_Number, t.Task_Description, t.Completion_Date CompletionDateTache, t.Task_Status
         , t.CreateurUser as CreateurTache, t.CreateurDate as CreateurDateTache
         , t.ModifUser as ModifyUserTache, t.ModifDate as ModifyDateTache
         , t.DateMinimale, t.DateMaximale
         , t.Task_Answer, t.Partenaire as PartenaireTache
         , t.Completion_Time as CompletionTimeTache
         , 'OUT' as ModifierType
         , -1 as ModifierInOut
         , a.Resolution_Description
         , a.Transaction_Type
         , Notes = CASE
                      WHEN t.Notes <> '' THEN t.Notes
                      WHEN t.Notes = '' AND n.Record_Notes is not null THEN n.Record_Notes
                      ELSE ''
                  END
         , a.SiCODebranche
         , ISNULL(ra.Description, 'S/O') as RaisonAnnulation
     FROM HS_Appel a
        LEFT JOIN hs_raisonannulation ra on a.RaisonAnnulation= ra.ID
         LEFT JOIN (select * from sv000805 aa  where note_author <> 'SA' and  (date1 + time1) = (select max(date1 + time1) from sv000805 ab where aa.service_call_id = ab.service_call_id )) n on n.Service_Call_ID = a.Service_Call_ID
         INNER JOIN HS_Tache t on a.Service_Call_ID= t.Service_Call_ID
          INNER JOIN sv00560 on sv00560.Task_Code= t.Task_Code
     WHERE ((t.Completion_Date >= CONVERT(DATETIME,'2016-11-09')
         AND t.Completion_Date < CONVERT(DATETIME,'2016-11-10')
        ) OR ( t.Completion_Date < CONVERT(DATETIME,'2016-11-10')
                  AND CreateurDate>= CONVERT(DATETIME,'2016-11-09')
                  AND CreateurDate < CONVERT(DATETIME,'2016-11-10')
        )) AND t.Task_Status = 'COMPLETE'
     UNION ALL
     SELECT A.CustNmbr, A.AdrsCode,  A.Service_CAll_ID, A.Type_of_Problem, A.Service_Description, A.Date_of_Service_Call
         , a.Status_of_Call, a.Completion_Date CompletionDateAppel, ga.Urgence
         , ga.Createur CreateurAppel, ga.ModifyUser as ModifyUserAppel
         , ga.ModifyDate as ModifyDateAppel, ga.UsagerResponsable as UsagerResponsableAppel
         , ga.CloserUser as CloserUserAppel, ga.CloserDate as CloserDateAppel
         , sv00560.Priority as PriorityTache
         , sv00560.TypeUsagerTache
         , t.Task_Code, t.Task_Line_Number, t.Task_Description, t.Completion_Date CompletionDateTache, t.Task_Status
         , gt.CreateurUser as CreateurTache, gt.CreateurDate as CreateurDateTache
         , gt.ModifUser as ModifyUserTache, gt.ModifDate as ModifyDateTache
         , gt.DateMinimale, gt.DateMaximale
         , gt.Task_Answer, gt.Partenaire as PartenaireTache
         , gt.Completion_Time as CompletionTimeTache
         , 'AVANT' as ModifierType
         , 1 as ModifierInOut
         , a.Resolution_Description
         , a.Transaction_Type
         , Notes = CASE
                      WHEN t.Notes <> '' THEN t.Notes
                      WHEN t.Notes = '' AND n.Record_Notes is not null THEN n.Record_Notes
                      ELSE ''
                  END
         , ga.SiCODebranche
         , ISNULL(ra.Description, 'S/O') as RaisonAnnulation
     FROM
         ( 
             SELECT A.CustNmbr, A.AdrsCode,  A.Service_CAll_ID, A.Type_of_Problem, A.Service_Description, A.Date_of_Service_Call
                 , a.Status_of_Call, a.Completion_Date, a.Resolution_Description, a.Transaction_Type
             FROM HS_Appel a
             WHERE Date_of_Service_Call < CONVERT(DATETIME,'2016-11-09')
                 AND Date_of_Service_Call >= CONVERT(DATETIME,'2015-11-10')
                 AND Status_of_Call = 'Ouv'
             UNION
             SELECT A.CustNmbr, A.AdrsCode,  A.Service_CAll_ID, A.Type_of_Problem, A.Service_Description, A.Date_of_Service_Call
                 , a.Status_of_Call, a.Completion_Date, a.Resolution_Description, a.Transaction_Type
             FROM HS_Appel a
             WHERE Date_of_Service_Call < CONVERT(DATETIME,'2016-11-09')
                 AND Date_of_Service_Call >= CONVERT(DATETIME,'2015-11-10')
                 AND Status_of_Call <> 'Ouv'
                 AND Completion_Date >= CONVERT(DATETIME,'2016-11-10')
         ) a
         INNER JOIN hs_Appel ga on a.Service_Call_ID= ga.Service_Call_id
            LEFT JOIN hs_raisonannulation ra on ga.RaisonAnnulation= ra.ID
         LEFT JOIN (select * from sv000805 aa  where note_author <> 'SA' and  (date1 + time1) = (select max(date1 + time1) from sv000805 ab where aa.service_call_id = ab.service_call_id )) n on n.Service_Call_ID = a.Service_Call_ID
         INNER JOIN
         (
             SELECT Service_call_Id,  Task_Code, Task_Line_Number, Task_Description, Completion_Date, Task_Status, Notes
             FROM HS_Tache WITH (INDEX (AKTask_Status))
             WHERE Task_Status <> 'COMPLETE'
             UNION
             SELECT Service_call_Id,  Task_Code, Task_Line_Number, Task_Description, Completion_Date, Task_Status, Notes
             FROM HS_Tache
             WHERE Completion_Date >= CONVERT(DATETIME,'2016-11-11')
                 AND Task_Status = 'COMPLETE'
         ) t   ON t.Service_Call_ID= a.Service_Call_ID
         INNER JOIN hs_tache gt on t.Service_Call_ID= gt.Service_Call_ID
             AND t.Task_Code= gt.Task_Code
             AND t.Task_Line_Number= gt.SeqNumbr
          INNER JOIN sv00560 on sv00560.Task_Code= t.Task_Code
     WHERE CreateurDate < CONVERT(DATETIME,'2016-11-09')
 ) w
 WHERE Type_Of_Problem IN ('**CORRECTIF**')"
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' Select  * from GMP_PARTENAIRES  where Traitement=''1'' and ID_Partenaire <>''''  and Resolution_Code<>''''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
"SELECT *
 INTO workTacheInOut_ed50617
 FROM (
     SELECT A.CustNmbr, A.AdrsCode,  A.Service_CAll_ID, A.Type_of_Problem, A.Service_Description, A.Date_of_Service_Call
         , a.Status_of_Call, a.Completion_Date CompletionDateAppel, a.Urgence
         , a.Createur CreateurAppel, a.ModifyUser as ModifyUserAppel
         , a.ModifyDate as ModifyDateAppel, a.UsagerResponsable as UsagerResponsableAppel
         , a.CloserUser as CloserUserAppel, a.CloserDate as CloserDateAppel
         , sv00560.Priority as PriorityTache
         , sv00560.TypeUsagerTache
         , t.Task_Code, t.Task_Line_Number, t.Task_Description, t.Completion_Date CompletionDateTache, t.Task_Status
         , t.CreateurUser as CreateurTache, t.CreateurDate as CreateurDateTache
         , t.ModifUser as ModifyUserTache, t.ModifDate as ModifyDateTache
         , t.DateMinimale, t.DateMaximale
         , t.Task_Answer, t.Partenaire as PartenaireTache
         , t.Completion_Time as CompletionTimeTache
         , 'IN' as ModifierType
         , 1 as ModifierInOut
         , a.Resolution_Description
         , a.Transaction_Type
         , Notes = CASE
                      WHEN t.Notes <> '' THEN t.Notes
                      WHEN t.Notes = '' AND n.Record_Notes is not null THEN n.Record_Notes
                      ELSE ''
                  END
         , a.SiCODebranche
         , ISNULL(ra.Description, 'S/O') as RaisonAnnulation
     FROM HS_Appel a
        LEFT JOIN hs_raisonannulation ra on a.RaisonAnnulation= ra.ID
         LEFT JOIN (select * from sv000805 aa  where note_author <> 'SA' and  (date1 + time1) = (select max(date1 + time1) from sv000805 ab where aa.service_call_id = ab.service_call_id )) n on n.Service_Call_ID = a.Service_Call_ID
         INNER JOIN HS_Tache t on a.Service_Call_ID= t.Service_Call_ID
          INNER JOIN sv00560 on sv00560.Task_Code= t.Task_Code
     WHERE CreateurDate>= CONVERT(DATETIME,'2016-11-09')
         AND CreateurDate < CONVERT(DATETIME,'2016-11-10')
     UNION ALL
     SELECT A.CustNmbr, A.AdrsCode,  A.Service_CAll_ID, A.Type_of_Problem, A.Service_Description, A.Date_of_Service_Call
         , a.Status_of_Call, a.Completion_Date CompletionDateAppel, a.Urgence
         , a.Createur CreateurAppel, a.ModifyUser as ModifyUserAppel
         , a.ModifyDate as ModifyDateAppel, a.UsagerResponsable as UsagerResponsableAppel
         , a.CloserUser as CloserUserAppel, a.CloserDate as CloserDateAppel
         , sv00560.Priority as PriorityTache
         , sv00560.TypeUsagerTache
         , t.Task_Code, t.Task_Line_Number, t.Task_Description, t.Completion_Date CompletionDateTache, t.Task_Status
         , t.CreateurUser as CreateurTache, t.CreateurDate as CreateurDateTache
         , t.ModifUser as ModifyUserTache, t.ModifDate as ModifyDateTache
         , t.DateMinimale, t.DateMaximale
         , t.Task_Answer, t.Partenaire as PartenaireTache
         , t.Completion_Time as CompletionTimeTache
         , 'OUT' as ModifierType
         , -1 as ModifierInOut
         , a.Resolution_Description
         , a.Transaction_Type
         , Notes = CASE
                      WHEN t.Notes <> '' THEN t.Notes
                      WHEN t.Notes = '' AND n.Record_Notes is not null THEN n.Record_Notes
                      ELSE ''
                  END
         , a.SiCODebranche
         , ISNULL(ra.Description, 'S/O') as RaisonAnnulation
     FROM HS_Appel a
        LEFT JOIN hs_raisonannulation ra on a.RaisonAnnulation= ra.ID
         LEFT JOIN (select * from sv000805 aa  where note_author <> 'SA' and  (date1 + time1) = (select max(date1 + time1) from sv000805 ab where aa.service_call_id = ab.service_call_id )) n on n.Service_Call_ID = a.Service_Call_ID
         INNER JOIN HS_Tache t on a.Service_Call_ID= t.Service_Call_ID
          INNER JOIN sv00560 on sv00560.Task_Code= t.Task_Code
     WHERE ((t.Completion_Date >= CONVERT(DATETIME,'2016-11-09')
         AND t.Completion_Date < CONVERT(DATETIME,'2016-11-10')
        ) OR ( t.Completion_Date < CONVERT(DATETIME,'2016-11-10')
                  AND CreateurDate>= CONVERT(DATETIME,'2016-11-09')
                  AND CreateurDate < CONVERT(DATETIME,'2016-11-10')
        )) AND t.Task_Status = 'COMPLETE'
     UNION ALL
     SELECT A.CustNmbr, A.AdrsCode,  A.Service_CAll_ID, A.Type_of_Problem, A.Service_Description, A.Date_of_Service_Call
         , a.Status_of_Call, a.Completion_Date CompletionDateAppel, ga.Urgence
         , ga.Createur CreateurAppel, ga.ModifyUser as ModifyUserAppel
         , ga.ModifyDate as ModifyDateAppel, ga.UsagerResponsable as UsagerResponsableAppel
         , ga.CloserUser as CloserUserAppel, ga.CloserDate as CloserDateAppel
         , sv00560.Priority as PriorityTache
         , sv00560.TypeUsagerTache
         , t.Task_Code, t.Task_Line_Number, t.Task_Description, t.Completion_Date CompletionDateTache, t.Task_Status
         , gt.CreateurUser as CreateurTache, gt.CreateurDate as CreateurDateTache
         , gt.ModifUser as ModifyUserTache, gt.ModifDate as ModifyDateTache
         , gt.DateMinimale, gt.DateMaximale
         , gt.Task_Answer, gt.Partenaire as PartenaireTache
         , gt.Completion_Time as CompletionTimeTache
         , 'AVANT' as ModifierType
         , 1 as ModifierInOut
         , a.Resolution_Description
         , a.Transaction_Type
         , Notes = CASE
                      WHEN t.Notes <> '' THEN t.Notes
                      WHEN t.Notes = '' AND n.Record_Notes is not null THEN n.Record_Notes
                      ELSE ''
                  END
         , ga.SiCODebranche
         , ISNULL(ra.Description, 'S/O') as RaisonAnnulation
     FROM
         ( 
             SELECT A.CustNmbr, A.AdrsCode,  A.Service_CAll_ID, A.Type_of_Problem, A.Service_Description, A.Date_of_Service_Call
                 , a.Status_of_Call, a.Completion_Date, a.Resolution_Description, a.Transaction_Type
             FROM HS_Appel a
             WHERE Date_of_Service_Call < CONVERT(DATETIME,'2016-11-09')
                 AND Date_of_Service_Call >= CONVERT(DATETIME,'2015-11-10')
                 AND Status_of_Call = 'Ouv'
             UNION
             SELECT A.CustNmbr, A.AdrsCode,  A.Service_CAll_ID, A.Type_of_Problem, A.Service_Description, A.Date_of_Service_Call
                 , a.Status_of_Call, a.Completion_Date, a.Resolution_Description, a.Transaction_Type
             FROM HS_Appel a
             WHERE Date_of_Service_Call < CONVERT(DATETIME,'2016-11-09')
                 AND Date_of_Service_Call >= CONVERT(DATETIME,'2015-11-10')
                 AND Status_of_Call <> 'Ouv'
                 AND Completion_Date >= CONVERT(DATETIME,'2016-11-10')
         ) a
         INNER JOIN hs_Appel ga on a.Service_Call_ID= ga.Service_Call_id
            LEFT JOIN hs_raisonannulation ra on ga.RaisonAnnulation= ra.ID
         LEFT JOIN (select * from sv000805 aa  where note_author <> 'SA' and  (date1 + time1) = (select max(date1 + time1) from sv000805 ab where aa.service_call_id = ab.service_call_id )) n on n.Service_Call_ID = a.Service_Call_ID
         INNER JOIN
         (
             SELECT Service_call_Id,  Task_Code, Task_Line_Number, Task_Description, Completion_Date, Task_Status, Notes
             FROM HS_Tache WITH (INDEX (AKTask_Status))
             WHERE Task_Status <> 'COMPLETE'
             UNION
             SELECT Service_call_Id,  Task_Code, Task_Line_Number, Task_Description, Completion_Date, Task_Status, Notes
             FROM HS_Tache
             WHERE Completion_Date >= CONVERT(DATETIME,'2016-11-11')
                 AND Task_Status = 'COMPLETE'
         ) t   ON t.Service_Call_ID= a.Service_Call_ID
         INNER JOIN hs_tache gt on t.Service_Call_ID= gt.Service_Call_ID
             AND t.Task_Code= gt.Task_Code
             AND t.Task_Line_Number= gt.SeqNumbr
          INNER JOIN sv00560 on sv00560.Task_Code= t.Task_Code
     WHERE CreateurDate < CONVERT(DATETIME,'2016-11-09')
 ) w
 WHERE Type_Of_Problem IN ('**CORRECTIF**')"
"CREATE NONCLUSTERED INDEX [IXW_SCID] ON workTacheInOut_ed50617
     [Service_Call_Id] Asc
"CREATE NONCLUSTERED INDEX [IXW_SCID] ON workTacheInOut_ed50617
     [Service_Call_Id] Asc
"DELETE FROM workTacheInOut_ed50617
 WHERE CUSTNMBR = '' or AdrsCode= '' or Service_Call_ID= ''"
"DELETE FROM workTacheInOut_ed50617
 WHERE CUSTNMBR = '' or AdrsCode= '' or Service_Call_ID= ''"
"DELETE FROM workTacheInOut_ed50617
 WHERE Task_Code= 'U0004'"
"DELETE FROM workTacheInOut_ed50617
 WHERE Task_Code= 'U0004'"
"INSERT INTO workTacheInOut_ed50617
 SELECT w.[CustNmbr]
       , w.[AdrsCode]
       , w.[Service_CAll_ID]
       , w.[Type_of_Problem]
       , w.[Service_Description]
       , w.[Date_of_Service_Call]
       , w.[Status_of_Call]
       , w.[CompletionDateAppel]
       , w.[Urgence]
       , w.[CreateurAppel]
       , w.[ModifyUserAppel]
       , w.[ModifyDateAppel]
       , w.[UsagerResponsableAppel]
       , w.[CloserUserAppel]
       , w.[CloserDateAppel]
       , w.[PriorityTache]
       , w.[TypeUsagerTache]
       , w.[Task_Code]
       , w.[Task_Line_Number]
       , w.[Task_Description]
       , w.[CompletionDateTache]
       , w.[Task_Status]
       , w.[CreateurTache]
       , w.[CreateurDateTache]
       , w.[ModifyUserTache]
       , w.[ModifyDateTache]
       , w.[DateMinimale]
       , w.[DateMaximale]
       , w.[Task_Answer]
       , w.[PartenaireTache]
       , w.[CompletionTimeTache]
       , 'AVANT'
       , 1
       , w.Resolution_Description
       , w.Transaction_Type
       , w.Notes
       , w.SiCODebranche
       , w.RaisonAnnulation
 FROM workTacheInOut_ed50617 w
     LEFT JOIN workTacheInOut_ed50617 w2 on w.Service_CAll_Id= w2.Service_Call_id
         AND w.Task_Code= w2.Task_Code
         AND w.Task_Line_Number= w2.Task_Line_Number
         AND w.ModifierType <> w2.ModifierType
 WHERE w.ModifierType= 'OUT'
     AND w2.ModifierType IS NULL"
"INSERT INTO workTacheInOut_ed50617
 SELECT w.[CustNmbr]
       , w.[AdrsCode]
       , w.[Service_CAll_ID]
       , w.[Type_of_Problem]
       , w.[Service_Description]
       , w.[Date_of_Service_Call]
       , w.[Status_of_Call]
       , w.[CompletionDateAppel]
       , w.[Urgence]
       , w.[CreateurAppel]
       , w.[ModifyUserAppel]
       , w.[ModifyDateAppel]
       , w.[UsagerResponsableAppel]
       , w.[CloserUserAppel]
       , w.[CloserDateAppel]
       , w.[PriorityTache]
       , w.[TypeUsagerTache]
       , w.[Task_Code]
       , w.[Task_Line_Number]
       , w.[Task_Description]
       , w.[CompletionDateTache]
       , w.[Task_Status]
       , w.[CreateurTache]
       , w.[CreateurDateTache]
       , w.[ModifyUserTache]
       , w.[ModifyDateTache]
       , w.[DateMinimale]
       , w.[DateMaximale]
       , w.[Task_Answer]
       , w.[PartenaireTache]
       , w.[CompletionTimeTache]
       , 'AVANT'
       , 1
       , w.Resolution_Description
       , w.Transaction_Type
       , w.Notes
       , w.SiCODebranche
       , w.RaisonAnnulation
 FROM workTacheInOut_ed50617 w
     LEFT JOIN workTacheInOut_ed50617 w2 on w.Service_CAll_Id= w2.Service_Call_id
         AND w.Task_Code= w2.Task_Code
         AND w.Task_Line_Number= w2.Task_Line_Number
         AND w.ModifierType <> w2.ModifierType
 WHERE w.ModifierType= 'OUT'
     AND w2.ModifierType IS NULL"
"UPDATE workTacheInOut_ed50617
     SET UsagerResponsableAppel = gt2.Task_Answer
 FROM workTacheInOut_ed50617
 INNER JOIN
     (
     SELECT  w.Service_Call_ID, w.Task_Code, w.Task_Status, w.Task_Line_Number
         , UsagerResponsableAppel, MAX(gt2.CreateurDate) as MaxCreateurDate
     FROM workTacheInOut_ed50617 w
         INNER JOIN HS_Tache gt2 on w.Service_Call_ID = gt2.Service_Call_ID
             AND gt2.Task_Code= 'U0004'
             AND gt2.Task_Answer <> ''
             AND gt2.ModifDate <= w.ModifyDateTache
     WHERE w.Task_Status= 'COMPLETE'
     GROUP BY w.Service_Call_ID, w.Task_Code, w.Task_Status, w.Task_Line_Number, UsagerResponsableAppel
 ) w ON w.Service_Call_ID= workTacheInOut_ed50617.Service_Call_ID
     AND w.Task_Code= workTacheInOut_ed50617.Task_Code
     AND w.Task_Line_Number= workTacheInOut_ed50617.Task_Line_Number
 INNER JOIN HS_Tache gt2 on w.Service_Call_ID = gt2.Service_Call_ID
             AND gt2.Task_Code= 'U0004'
             AND gt2.Task_Answer <> ''
             AND gt2.CreateurDate = w.MaxCreateurDate
 WHERE w.Task_Status= 'COMPLETE'
     AND gt2.Task_Answer <> workTacheInOut_ed50617.UsagerResponsableAppel"
"UPDATE workTacheInOut_ed50617
     SET UsagerResponsableAppel = gt2.Task_Answer
 FROM workTacheInOut_ed50617
 INNER JOIN
     (
     SELECT  w.Service_Call_ID, w.Task_Code, w.Task_Status, w.Task_Line_Number
         , UsagerResponsableAppel, MAX(gt2.CreateurDate) as MaxCreateurDate
     FROM workTacheInOut_ed50617 w
         INNER JOIN HS_Tache gt2 on w.Service_Call_ID = gt2.Service_Call_ID
             AND gt2.Task_Code= 'U0004'
             AND gt2.Task_Answer <> ''
             AND gt2.ModifDate <= w.ModifyDateTache
     WHERE w.Task_Status= 'COMPLETE'
     GROUP BY w.Service_Call_ID, w.Task_Code, w.Task_Status, w.Task_Line_Number, UsagerResponsableAppel
 ) w ON w.Service_Call_ID= workTacheInOut_ed50617.Service_Call_ID
     AND w.Task_Code= workTacheInOut_ed50617.Task_Code
     AND w.Task_Line_Number= workTacheInOut_ed50617.Task_Line_Number
 INNER JOIN HS_Tache gt2 on w.Service_Call_ID = gt2.Service_Call_ID
             AND gt2.Task_Code= 'U0004'
             AND gt2.Task_Answer <> ''
             AND gt2.CreateurDate = w.MaxCreateurDate
 WHERE w.Task_Status= 'COMPLETE'
     AND gt2.Task_Answer <> workTacheInOut_ed50617.UsagerResponsableAppel"
"UPDATE workTacheInOut_ed50617
     SET UsagerResponsableAppel = '*** NON D
 ***'
 FROM workTacheInOut_ed50617
 WHERE ISNULL(RTRIM(UsagerResponsableAppel), '') = ''"
"UPDATE workTacheInOut_ed50617
     SET UsagerResponsableAppel = '*** NON D
 ***'
 FROM workTacheInOut_ed50617
 WHERE ISNULL(RTRIM(UsagerResponsableAppel), '') = ''"
"UPDATE workTacheInOut_ed50617
     SET Resolution_Description = '*** AUCUN ***'
 FROM workTacheInOut_ed50617
 WHERE ISNULL(RTRIM(Resolution_Description), '') = ''"
"UPDATE workTacheInOut_ed50617
     SET Resolution_Description = '*** AUCUN ***'
 FROM workTacheInOut_ed50617
 WHERE ISNULL(RTRIM(Resolution_Description), '') = ''"
"UPDATE workTacheInOut_ed50617
     SET ModifyUserTache = '*** S/O ***'
 FROM workTacheInOut_ed50617
 WHERE ISNULL(RTRIM(ModifyUserTache), '') = ''"
"UPDATE workTacheInOut_ed50617
     SET ModifyUserTache = '*** S/O ***'
 FROM workTacheInOut_ed50617
 WHERE ISNULL(RTRIM(ModifyUserTache), '') = ''"
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'
 SELECT RTRIM(w.CustNmbr) [# Client]
       , RTRIM(w.AdrsCode) [Code adresse]
       , RTRIM(w.Service_CAll_ID) [# appel]
       , RTRIM(w.Type_of_Problem) [Type appel]
       , RTRIM(w.Service_Description) [Description appel]
       , w.Date_of_Service_Call [Date appel]
       , RTRIM(w.Status_of_Call) [Statut appel]
       , w.CompletionDateAppel [Date appel compl
       , w.ModifyDateAppel [Date appel modifi
       , w.CloserDateAppel [Date fermeture appel]
       , PriorityTache [Priorit
 de la t
che]
       , TypeUsagerTache [Type d''usager]
       , RTRIM(Task_Code) [Code de t
che]
       , Task_Line_Number [# Ligne t
che]
       , RTRIM(Task_Description) [Description t
che]
       , CompletionDateTache [Date t
che compl
       , RTRIM(Task_Status) [Statut t
che]
       , CreateurDateTache [Date cr
ation t
che]
       , ModifyDateTache [Date t
che modifi
       , w.DateMinimale [Date minimale t
che]
       , w.DateMaximale [Date maximale t
che]
       , CASE WHEN w.DateMinimale <= GETDATE() THEN 1 ELSE 0 END [
chue]
       , CASE WHEN w.DateMinimale > GETDATE() THEN 1 ELSE 0 END [
ch. futurs]
       , RTRIM(Task_Answer) [R
ponse t
che]
       , RTRIM(PartenaireTache) [Partenaire t
che]
       , CompletionTimeTache [Heure t
che compl
       , ModifierType [Type Diff
rentiel t
che]
       , ModifierInOut [Diff
rentiel t
che]
       , ISNULL(RTRIM(UPPER(LTRIM(uDelegue.LName + '' '') + uDelegue.fname + '' ('' + uDelegue.UsagerID + '')'')), ''*** NON D
 ***'') as [Usager D
       , ISNULL(RTRIM(UPPER(LTRIM(uModifyUserTache.LName + '' '') + uModifyUserTache.fname + '' ('' + uModifyUserTache.UsagerID + '')'')), ''*** S/O ***'') as [Usager t
che modifi
       , ISNULL(RTRIM(UPPER(LTRIM(uCreateurTache.LName + '' '') + uCreateurTache.fname + '' ('' + uCreateurTache.UsagerID + '')'')), ''*** S/O ***'') as [Usager cr
ateur t
che]
       , ISNULL(RTRIM(UPPER(LTRIM(uCreateurAppel.LName + '' '') + uCreateurAppel.fname + '' ('' + uCreateurAppel.UsagerID + '')'')), ''*** S/O ***'') as [Usager cr
ateur appel]
       , ISNULL(RTRIM(UPPER(LTRIM(uCloserUserAppel.LName + '' '') + uCloserUserAppel.fname + '' ('' + uCloserUserAppel.UsagerID + '')'')), ''*** S/O ***'') as [Usager fermeture appel]
       , ISNULL(RTRIM(UPPER(LTRIM(uModifyUserAppel.LName + '' '') + uModifyUserAppel.fname + '' ('' + uModifyUserAppel.UsagerID + '')'')), ''*** S/O ***'') as [Usager appel modifi
       , ISNULL(RTRIM(w.Resolution_Description), '''') as [R
solution Appel]
      , ISNULL(rm00103.CustBlnc, 0) as [Solde Client]
      , CASE WHEN ISNULL(w.Urgence, 0)= 1 THEN ''OUI'' ELSE ''NON'' END as [Urgence]
      , ISNULL(gmp_batiment.CGE_Description, ''*** INCONNU ***'') as [Type B
timent]
      , ISNULL(p.Contract_Number, ''S/O'') as [# Contrat li
      , ISNULL(p.Contract_Internal_Name, ''S/O'') as [Type Contrat li
      , ISNULL(w.Transaction_Type, ''S/O'') as [Type de transaction]
      , w.Notes as [Notes]
      , w.SiCODebranche [Si Ch-O D
branch
      , DATEDIFF(YY,e.install_date,GETDATE()) as [
ge du chauffe-eau]
      , w.RaisonAnnulation as [Raison Annulation]
 FROM workTacheInOut_ed50617 w
   LEFT JOIN gmp_usagers uDelegue on w.UsagerResponsableAppel= uDelegue.UsagerID
   LEFT JOIN gmp_usagers uModifyUserTache on w.ModifyUserTache= uModifyUserTache.UsagerID
   LEFT JOIN gmp_usagers uCreateurTache on w.CreateurTache= uCreateurTache.UsagerID
   LEFT JOIN gmp_usagers uCreateurAppel on w.CreateurAppel= uCreateurAppel.UsagerID
   LEFT JOIN gmp_usagers uCloserUserAppel on w.CloserUserAppel= uCloserUserAppel.UsagerID
   LEFT JOIN gmp_usagers uModifyUserAppel on w.ModifyUserAppel= uModifyUserAppel.UsagerID
   LEFT JOIN HS_Client c on c.CustNmbr= w.CustNmbr
      AND c.AdrsCode= w.ADrsCode
   LEFT JOIN gmp_Batiment ON gmp_batiment.pro_cat_bat= c.pro_cat_bat
   LEFT JOIN rm00103 ON rm00103.CustNmbr = c.CompteRecevoir
   LEFT JOIN hs_Appel a on a.Service_Call_ID= w.Service_Call_ID
   LEFT JOIN hs_produit p on p.CustNmbr= a.CustNmbr
        AND p.AdrsCode= a.AdrsCode
        AND p.Contract_Number= a.Contract_number
   LEFT JOIN hs_equipement e on a.Equipment_ID = e.Equipment_ID',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' Select  * from GMP_PARTENAIRES  where Traitement=''1'' and ID_Partenaire <>''''  and Resolution_Code<>''''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
DROP TABLE workTacheInOut_ed50617
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
INSERT INTO [dbo].[HS_Log_ImpressionRapports]            ([DateDemande]            ,[Usager]            ,[FonctionRapport]            ,[Arguments])      Values            (GETDATE()            ,'ed50617'            ,'RptCall_ListeAppelsUrgent'            ,'2016-10-10 \\ 2016-11-10'            )
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT a.CustNmbr [# Client]     , a.AdrsCode [Code Adresse]     , a.Service_Call_ID [# Appel]     , Type_Of_Problem [Type de problme]     , Date_of_Service_Call [Date Appel]     , Time_Of_Service_Call [Heure Appel]     , Status_of_Call [Statut Appel]     , Completion_Date [Date compl]     , UsagerResponsable [Usager responsable] FROM HS_Appel a WHERE Date_of_Service_Call >= CONVERT(DATETIME,''2016-10-10'')     AND Date_of_Service_Call <= CONVERT(DATETIME,''2016-11-10'')     AND a.Urgence= 1',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
INSERT INTO [dbo].[HS_Log_ImpressionRapports]            ([DateDemande]            ,[Usager]            ,[FonctionRapport]            ,[Arguments])      Values            (GETDATE()            ,'ed50617'            ,'RptCall_FacturationGMI'            ,'2016-11-10 \\ 0'            )
INSERT INTO [dbo].[HS_Log_ImpressionRapports]            ([DateDemande]            ,[Usager]            ,[FonctionRapport]            ,[Arguments])      Values            (GETDATE()            ,'ed50617'            ,'RptCall_FacturationGMI'            ,'2016-11-10 \\ 0'            )
exec sp_cursoropen @p1 output,N'  select * from GMI_PERIODE  where 1 = 1  and CONVERT(DATETIME,''2016-11-10'')  between DebutPeriode and FinPeriode',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' Select  * from GMP_PARTENAIRES  where Traitement=''1'' and ID_Partenaire <>''''  and Resolution_Code<>''''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' Select  * from GMP_PARTENAIRES  where Traitement=''1'' and ID_Partenaire <>''''  and Resolution_Code<>''''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' Select  * from GMP_PARTENAIRES  where Traitement=''1'' and ID_Partenaire <>''''  and Resolution_Code<>''''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentWebServices FROM gmp_systeme',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem IN (''SOUMISSION CHAUFFAGE'', ''SOUMISSION CLIMATISATION'')',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT *  FROM HS_APPEL  WHERE USERID= ''WebServices''   AND Type_Of_Problem = ''REPARATION DU CHAUFFE-EAU''',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N' SELECT ea.*, ear.EigenResponseMsg, ear.EigenWsAgentStatus  FROM gmp_eigen_agent_WsResp ear   INNER JOIN gmp_eigen_agent ea on ea.InvoiceID= ear.InvoiceID WHERE EigenWsAgentStatus= 0',@p3 output,@p4 output,@p5 output
exec sp_cursoropen @p1 output,N'Select StopAgentPartenaires FROM gmp_systeme',@p3 output,@p4 output,@p5 output
