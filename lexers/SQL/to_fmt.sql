SELECT c.session , c.username ,TIMEDIFF(max(c.datetime) , min(c.datetime))
FROM  livehelp_domain_user a , livehelp_users b, livehelp_messages c
WHERE a.id_domain = 29 AND
      a.id_user = b.id AND
      CONCAT( b.firstname, ' ',b.lastname)   = c.username AND
      DATE_FORMAT(c.datetime,'%m/%d/%Y') ='09/21/2006' AND
      (SELECT COUNT(data) from sometable where Fld = 'YES') = 6 
GROUP BY c.session
ORDER BY c.session,c.username

