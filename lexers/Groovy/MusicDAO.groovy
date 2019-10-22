package com.mg.groovy.samples.db.infra

/**
 * MusicDAO.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class MusicDAO extends DataAccessObject{
	
	List getFields() { 
		return [ 
		'artist',    'VARCHAR(64)', 
		'name',     'VARCHAR(64)', 
		'album',     'VARCHAR(64)',
		'date_launched',  'DATE' 
		] 
	} 
	
}
