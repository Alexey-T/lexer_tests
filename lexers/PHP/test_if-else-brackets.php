<?
function MainFrame(&$db, $skin) {

	HTML_Page::HTML_Page($db, $skin, array(
		'Body', 'TopMenu', 'TopMenu2', 'News', 'News_first', 'Caption','Goods', 'RightBlock','ModMenu','SphMenu',
		'RegionMenu','Last','Rand','Popular', 'LastAnk','banner1','banner2','banner3','banner4','topgirl','Search',
		'Menu', 'KontaktMain','LastComment'
	));


	if($_GET['go'] == 'print'){
		$this->addCssFile("print.css");
	}
	else{
		$this->addCssFile("/template_styles.css");
		$this->addCssFile("/fancybox/jquery.fancybox.css");
	}
}
?>