<?
Error_Reporting(E_ALL & ~E_NOTICE);

include "test.php";
include("test2.php");
include_once "test.php";
include_once("test2.php");

require_once("define.php");
require_once("lib/PEAR.php");

require_once("lib/template.php");
require_once("lib/buttons.php");
require_once("lib/HTML_Page.php");
require_once("lib/DB_Table.php");
//require_once("lib/components.php");
require_once("lib/HTML/Common/Common.php");
//require_once("lib/HTML/FormElements/select.php");
//require_once("lib/HTML/FormElements/text.php");
//require_once("lib/HTML/FormElements/hidden.php");
//require_once("lib/HTML/FormElements/checkbox.php");
//require_once("lib/HTML/FormElements/textarea.php");
//require_once("lib/HTML/FormElements/label.php");
# include  database lib
require_once("lib/DB/DB.php");
require_once('lib/DB/DB/common.php');
require_once('lib/DB/DB/mysql.php');
require_once("lib/HTML/Table/Table.php");
require_once("lib/HTML/Form/Form.php");
require_once("lib/HTML/Sliding/Sliding.php");

require_once("lib/Auth.php");
require_once("main.php");
#
//require_once("editor/fckeditor.php");
if (!isset($db)) {

    class my_DB extends DB {

        function connect($phptype = 'mysql') {
            $dsn = "mysql://" . DB_USER . ":" . DB_PASSWORD . "@" . DB_SERVER . "/" . DB_DATABASE;
            $op = array(
                'debug' => 2,
                'portability' => DB_PORTABILITY_ALL,
            );

            //PEAR::setErrorHandling(PEAR_ERROR_TRIGGER);

            $db = & parent::connect($dsn, $op);

            if (parent::isError($db)) {
                die($db->getMessage());
            }
            return $db;
        }

    }

    $db = &my_DB::connect();
    $db->query("SET NAMES cp1251");
}

$page = null;
$part_id = isset($_GET['part_id']) ? $_GET['part_id'] : '1';
$array = explode(",", $part_id);
$part_id = intval($array[count($array) - 1]);

$sql = "SELECT *
          FROM ref_parts
          INNER JOIN ref_patterns USING (pattern_id)
          WHERE ref_parts.part_id=$part_id";
$row = $db->getRow($sql, array(), DB_FETCHMODE_ASSOC);


if (!empty($_GET['go'])) {
    switch ($_GET['go']) {
        case 'winefilter':
            require_once("wine-filter.php");
            //$page = &new WineFilterPage($db, "templates/maintwo.tpl");
            $page = &new WineFilterPage($db, "templates/mainthree.tpl");
            break;
        case 'catalog':
            require_once("catalog.php");
            $page = &new CatalogPage($db, "templates/maintwo.tpl");
            break;
        case 'map':
            require_once("map.php");
            $page = &new MapPage($db, "templates/maintwo.tpl");
            break;
        case 'goods':
            require_once("goods.php");
            $page = &new GoodsPage($db, "templates/maintwo.tpl");
            break;
        case 'order':
            require_once("order.php");
            $page = &new OrderPage($db, "templates/maintwo.tpl");
            break;
        case 'search':
            require_once("catalog_search.php");
            //$page = &new CatalogSearchPage($db, "templates/maintwo.tpl");
            $page = &new CatalogSearchPage($db, "templates/mainthree.tpl");
            break;
        /*
        case 'search':
            require_once("search.php");
            $page = &new SearchPage($db, "templates/maintwo.tpl");
            break;
         * 
         */
        case 'users':
            require_once("users.php");
            $page = &new UsersPage($db, "templates/maintwo.tpl");
            break;
    }
} elseif (!is_array($row) || $part_id == 1) {
    require_once("home.php");
    $page = &new HomePage($db, "templates/mainone.tpl");
    $page->_part_id = 1;
} else {
    switch ($row['pattern_name']) {
        case 'none':
            require_once("default.php");
            $page = &new DefaultPage($db, "templates/maintwo.tpl");
            break;
        case 'catalog':
            require_once("catalog.php");
            $page = &new CatalogPage($db, "templates/maincat.tpl");
            break;
        case 'prices':
            require_once("prices.php");
            $page = &new PricesPage($db, "templates/maintwo.tpl");
            break;
        case 'news':
            require_once("news.php");
            $page = &new NewsPage($db, "templates/mainthree.tpl");
            break;
        case 'goods':
            require_once("goods.php");
            $page = &new GoodsPage($db, "templates/maintwo.tpl");
            break;
        case 'photos':
            require_once("photos.php");
            $page = &new PhotosPage($db, "templates/maintwo.tpl");
            break;
        case 'question':
            require_once("question.php");
            $page = &new QuestionPage($db, "templates/mainthree.tpl");
            break;
        case 'articles':
            require_once("articles.php");
            $page = &new ArticlesPage($db, "templates/maintwo.tpl");
            break;
        default:
            require_once("home.php");
            $page = &new HomePage($db, "templates/maintwo.tpl");
            break;
    }
}

if (!is_null($page)) {
    $page->display();
}
?>