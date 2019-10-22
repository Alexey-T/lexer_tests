<?

require_once("DB_Table_Object.php");

class Parts extends DB_Table_Object {

    function Parts(&$db, $where) {
        parent::DB_Table_Object($db, 'ref_parts', 'part_id', 'part_order', $where);
        $this->setFormTemplate('templates/admin_parts_form.tpl');
    }

    function &getFormFields($op = 'edit') {
        $attr = array();
        $attr['part_name']['Type']  = 'text';
        $attr['part_name']['Label'] = 'Название раздела';
        $attr['part_name']['Attr']  = array('size'                        => 35);
        $attr['part_name']['Caption'] = 'Название раздела';


        $attr['part_parent_id']['Type']  = 'select';
        $attr['part_parent_id']['Label'] = 'Родительский узел';
        $attr['part_parent_id']['Attr']  = array('style'                            => 'width:230px');
        $attr['part_parent_id']['Query']   = "SELECT $this->_tbl_key, part_name FROM $this->_tbl";
        $attr['part_parent_id']['Options'] = array(array(0 => '0', 1 => MAIN_NODE));


        $attr['pattern_id']['Type']  = 'select';
        $attr['pattern_id']['Label'] = 'Тип шаблона';
        $attr['pattern_id']['Attr']  = array('style'                        => 'width:230px');
        $attr['pattern_id']['DB']      = 'ref_patterns';
        $attr['pattern_id']['Caption'] = 'Тип шаблона';

        $attr['structure_id']['Type']  = 'select';
        $attr['structure_id']['Label'] = 'Расположение';
        $attr['structure_id']['Attr']  = array('style'                          => 'width:230px');
        $attr['structure_id']['DB']      = 'ref_structures';
        $attr['structure_id']['Caption'] = 'Расположение';


        $attr['part_visible']['Type']    = 'checkbox';
        $attr['part_visible']['Caption'] = 'Видимость';
        $attr['part_visible']['Text']    = 'Видим для пользователя';
        $attr['part_visible']['Label']   = 'Видимость';
        //
        if ($op != 'edit') {
            $attr['part_push']['Type']  = 'select';
            $attr['part_push']['Label'] = 'Добавить';
            $attr['part_push']['Attr']  = array('style'                       => 'width:230px');
            $attr['part_push']['Options'] = array(array(0 => '0', 1 => 'Первым'), array(0 => '1', 1 => 'Последним'));
        } else {
            $attr['part_push']['Type']      = 'hidden';
            $attr['part_push']['Label']     = '';
        }
        //
        $attr['part_add_info']['Type']  = 'editor';
        $attr['part_add_info']['Label'] = 'Содержание раздела';
        $attr['part_add_info']['Attr']  = array('width'  => '100%', 'height' => '400px');

        // set folders path
        require_once("admin_explorerfiles.php");
        $folders = ExplorerFPage::_getFolders("../", 0, array("../admin", '../lib', '../editor'));
        $paths = array();
        foreach ($folders as $folder) {
            $paths[] = array(0                                 => $folder['path'], 1                                 => $folder['path']);
        }
        // set image path
        $attr['part_image_path']['Type']  = 'select';
        $attr['part_image_path']['Label'] = 'Путь к картинкам';
        $attr['part_image_path']['Attr']  = array('style'                              => 'width:230px');
        $attr['part_image_path']['Options']  = $paths;
        $attr['part_image_path']['Selected'] = '../Image';
        // set docs path
        $attr['part_docs_path']['Type']      = 'select';
        $attr['part_docs_path']['Label']     = 'Путь к файлам';
        $attr['part_docs_path']['Attr']      = array('style'                             => 'width:230px');
        $attr['part_docs_path']['Options']  = $paths;
        $attr['part_docs_path']['Selected'] = '../File';
        return $attr;
    }

    function insert($values, $type = 'insert') {
        global $user;

        if ($user->getType() != 1) {
            exit('Запрещенная операция. Об инциденте сообщено администратору.');
        }

        if ($type == 'edit') {
            $values['part_visible'] = isset($values['part_visible']) ? 1 : 0;
        }
        //
        $pos                    = 1;
        if (isset($values['part_push'])) {
            if (intval($values['part_push']) > 0) {
                $sql = "SELECT *FROM $this->_tbl
				        WHERE $this->_where
				        ORDER BY $this->_tbl_ordering DESC LIMIT 1";
                $row = $this->_db->getRow($sql, array(), DB_FETCHMODE_ASSOC);
                $pos                 = $row[$this->_tbl_ordering] + 1;
            }
            unset($values['part_push']);
        }
        $values['part_date'] = date("Y/m/d");
        parent::insert($values, $pos);
        $values              = array();
        if ($user->getType() > 1) {
            $sql = "SELECT $this->_tbl_key FROM $this->_tbl ORDER BY $this->_tbl_key DESC";
            $row = $this->_db->getRow($sql, array(), DB_FETCHMODE_ASSOC);
            $values['user_id'] = $user->getId();
            $values['part_id'] = $row['part_id'];
            insert($this->_db, 'ref_user_permissions', $values);
        }
        return true;
    }

    function update($values, $oid, $type = 'edit') {
        global $user;
        if ($user->getType() != 1) {
            exit('Запрещенная операция. Об инциденте сообщено администратору.');
        }

        if ($type == 'edit') {
            $values['part_visible'] = isset($values['part_visible']) ? 1 : 0;
        }
        if (isset($values['part_push'])) {
            unset($values['part_push']);
        }
        $values['part_date'] = date("Y/m/d");
        return parent::update($values, $oid);
    }

    function publish($oid) {
        global $user;
        if ($user->getType() != 1) {
            exit('Запрещенная операция. Об инциденте сообщено администратору.');
        }

        $values = array();
        $values['part_visible'] = 1;
        return parent::update($values, $oid, 'publish');
    }

    function unpublish($oid) {
        global $user;
        if ($user->getType() != 1) {
            exit('Запрещенная операция. Об инциденте сообщено администратору.');
        }

        $values = array();
        $values['part_visible'] = 0;
        return parent::update($values, $oid, 'unpublish');
    }

    function delete($oid) {
        global $user;
        if ($user->getType() != 1) {
            exit('Запрещенная операция. Об инциденте сообщено администратору.');
        }

        $sql = "SELECT *FROM ref_parts WHERE part_parent_id = $oid";
        $row = $this->_db->getRow($sql, array(), DB_FETCHMODE_ASSOC);
        if (is_array($row)) {
            return false;
        }
        $sql  = "SELECT *FROM ref_user_permissions WHERE part_id=" . $oid;
        $rows = $this->_db->getAll($sql, array(), DB_FETCHMODE_ASSOC);
        if (!is_null($rows)) {
            foreach ($rows as $row) {
                delete($this->_db, 'ref_user_permissions', 'part_id', $oid);
            }
        }
        return parent::delete($oid);
    }

    function getPattern($oid) {
        $sql = "SELECT *
	           FROM $this->_tbl
	           INNER JOIN ref_patterns USING (pattern_id)
	           WHERE $this->_tbl.part_id=" . $oid;
        $row = $this->_db->getRow($sql, array(), DB_FETCHMODE_ASSOC);
        return is_array($row) ? $row['pattern_name'] : null;
    }

}

class Articles extends DB_Table_Object {

    function Articles(&$db, $where) {
        parent::DB_Table_Object($db, 'ref_articles', 'articles_id', 'articles_order', $where);
        //$this->setFormTemplate('templates/admin_photos_form.tpl');
        $this->setCaption("Статьи", "images/news.png");
    }

    function insert($values) {

        $articles_date = $values['articles_date'];
        if (preg_match('/(\d{1,2})-(\d{1,2})-(\d{4})/i', $values['articles_date'], $m)) {
            $values['articles_date'] = $m[3] . "-" . $m[2] . "-" . $m[1];
        } else {
            $values['articles_date'] = date("Y-m-d");
        }

        return parent::insert($values);
    }

    function update($values, $oid) {
        if (preg_match('/(\d{1,2})-(\d{1,2})-(\d{4})/i', $values['articles_date'], $m)) {
            $values['articles_date'] = $m[3] . "-" . $m[2] . "-" . $m[1];
        } else {
            $values['articles_date'] = date("Y-m-d");
        }
        return parent::update($values, $oid);
    }

    function &getFormFields() {
        $attr = array();

        $attr['articles_date']['Type']  = 'date';
        $attr['articles_date']['Label'] = 'Дата';
        $attr['articles_date']['Attr']  = array('size'                            => 32);
        $attr['articles_date']['Caption'] = 'Дата';

        $attr['articles_image']['Type']  = 'image';
        $attr['articles_image']['Label'] = 'Картинка';


        $attr['articles_image_desc']['Type']  = 'text';
        $attr['articles_image_desc']['Label'] = 'Название';
        $attr['articles_image_desc']['Attr']  = array('size'                                  => 35);
        $attr['articles_image_desc']['Caption'] = 'Название';

        $attr['articles_resume']['Type']  = 'textarea';
        $attr['articles_resume']['Label'] = 'Резюме';
        $attr['articles_resume']['Attr']  = array('rows' => 6, 'cols' => 39);
//        $attr['articles_resume']['Caption'] = 'Резюме';

        $attr['articles_content']['Type']  = 'editor';
        $attr['articles_content']['Label'] = 'Описание';
        $attr['articles_content']['Attr']  = array('width'  => '100%', 'height' => '400px');

        $attr['part_id']['Type']  = 'hidden';
        $attr['part_id']['Label'] = '';
        return $attr;
    }

}

class Prices extends DB_Table_Object {

    function Prices(&$db, $where) {
        parent::DB_Table_Object($db, 'ref_prices', 'price_id', 'price_order', $where);
        //$this->setFormTemplate('templates/admin_photos_form.tpl');
        $this->setCaption("Документы", "images/docs.png");
    }

    function insert($values) {
        if (preg_match('/(\d{1,2})-(\d{1,2})-(\d{4})/i', $values['price_date'], $m)) {
            $values['price_date'] = $m[3] . "-" . $m[2] . "-" . $m[1];
        } else {
            $values['price_date'] = date("Y-m-d");
        }
        if (parent::insert($values)) {
            $res = $this->_db->getRow("SELECT *FROM ref_parts WHERE part_id=$values[part_id]", array(), DB_FETCHMODE_ASSOC);
            $res['part_docs_path'] .= (substr($res['part_docs_path'], -1) == "/") ? "" : "/";
            if (file_exists($res['part_docs_path'] . $values['price_file'])) {
                // create zip file
                gzip($res['part_docs_path'] . $values['price_file']);
                return true;
            }
        }
        return false;
    }

    function update($values, $oid) {
        global $user;
        if ($user->getType() == 1)
            if (preg_match('/(\d{1,2})-(\d{1,2})-(\d{4})/i', $values['price_date'], $m)) {
                $values['price_date'] = $m[3] . "-" . $m[2] . "-" . $m[1];
            } else {
                $values['price_date'] = date("Y-m-d");
            }
        if (parent::update($values, $oid)) {
            $res = $this->_db->getRow("SELECT *FROM ref_parts WHERE part_id=$values[part_id]", array(), DB_FETCHMODE_ASSOC);
            $res['part_docs_path'] .= (substr($res['part_docs_path'], -1) == "/") ? "" : "/";
            if (file_exists($res['part_docs_path'] . $values['price_file'])) {
                // create zip file
                gzip($res['part_docs_path'] . $values['price_file']);
                return true;
            }
        }
        return false;
    }

    function &getFormFields() {
        $attr = array();

        global $user;
        if ($user->getType() == 1) {
            $attr['price_date']['Type']    = 'date';
            $attr['price_date']['Label']   = 'Дата подписания';
            $attr['price_date']['Caption'] = 'Дата подписания';
            $attr['price_date']['Attr']    = array('size' => 32);
        }

        $attr['price_file']['Type']  = 'doc';
        $attr['price_file']['Label'] = 'Файл-вложение';

        $attr['price_name']['Type']  = 'text';
        $attr['price_name']['Label'] = 'Название';
        $attr['price_name']['Attr']  = array('size'                         => 35);
        $attr['price_name']['Caption'] = 'Название';


        $attr['part_id']['Type']  = 'hidden';
        $attr['part_id']['Label'] = '';
        return $attr;
    }

}

class Cena extends DB_Table_Object {

    function Cena(&$db, $where) {
        parent::DB_Table_Object($db, 'ref_cena', 'cena_id', 'cena_order', $where);
        $this->setCaption("Цены на материалы", "images/photos.png");
    }

    function &getFormFields() {
        $attr = array();
        $attr['cena_tolsh']['Type']  = 'text';
        $attr['cena_tolsh']['Label'] = 'толщина материала';
        $attr['cena_tolsh']['Attr']  = array('size'                         => 35);
        $attr['cena_tolsh']['Caption'] = 'толщина материала';
        $attr['cena_price']['Type']    = 'text';
        $attr['cena_price']['Label']   = 'Цена';
        $attr['cena_price']['Attr']    = array('size'                         => 35);
        $attr['cena_price']['Caption'] = 'Цена';
        $attr['part_id']['Type']       = 'hidden';
        $attr['part_id']['Label']      = '';
        return $attr;
    }

}

class Quest extends DB_Table_Object {

    function Quest(&$db, $where) {
        parent::DB_Table_Object($db, 'ref_quest', 'quest_id', 'quest_order', $where);
        //$this->setFormTemplate('templates/admin_photos_form.tpl');
        $this->setCaption("Вопрос-ответ", "images/links.png");
    }

    function insert($values, $type = 'insert') {
        $values['quest_visible'] = isset($values['quest_visible']) ? 1 : 0;



        if (preg_match('/(\d{1,2})-(\d{1,2})-(\d{4})/i', $values['quest_date'], $m)) {
            $values['quest_date'] = $m[3] . "-" . $m[2] . "-" . $m[1];
        } else {
            $values['quest_date'] = date("Y-m-d");
        }


        if (preg_match('/(\d{1,2})-(\d{1,2})-(\d{4})/i', $values['quest_dateq'], $m)) {
            $values['quest_dateq'] = $m[3] . "-" . $m[2] . "-" . $m[1];
        } else {
            $values['quest_dateq'] = date("Y-m-d");
        }

        return parent::insert($values);
    }

    function update($values, $oid) {
        $values['quest_visible'] = isset($values['quest_visible']) ? 1 : 0;

        if (preg_match('/(\d{1,2})-(\d{1,2})-(\d{4})/i', $values['quest_date'], $m)) {
            $values['quest_date'] = $m[3] . "-" . $m[2] . "-" . $m[1];
        } else {
            $values['quest_date'] = date("Y-m-d");
        }


        if (preg_match('/(\d{1,2})-(\d{1,2})-(\d{4})/i', $values['quest_dateq'], $m)) {
            $values['quest_dateq'] = $m[3] . "-" . $m[2] . "-" . $m[1];
        } else {
            $values['quest_dateq'] = date("Y-m-d");
        }

        if ($values['quest_send'] && $values["quest_mail"]) {
            $body = "
				<h4>Ответ на Ваш вопрос</h4>
				<br />
				<strong>Ваш вопрос:</strong> {$values[quest_question]}<br />
				<strong>Ответ:</strong> {$values[quest_comment]}<br /><br />
				С уважением,<br />
			";

            mail($values["quest_mail"], "Ответ на вопрос на сайте www.estet-center.ru", $body, "From:" . MAIL_TO . "\nContent-Type: text/html; charset=\"Windows-1251\"");
        }
        unset($values['quest_send']);
        return parent::update($values, $oid);
    }

    function &getFormFields() {
        $attr = array();

        $attr['quest_date']['Type']  = 'date';
        $attr['quest_date']['Label'] = 'Дата';
        $attr['quest_date']['Attr']  = array('size'                         => 32);
        $attr['quest_date']['Caption'] = 'Дата';

        /*
          $attr['quest_time']['Type']  = 'time';
          $attr['quest_time']['Label'] = 'Время';
          $attr['quest_time']['Attr'] = array('size' => 32);
          $attr['quest_time']['Caption'] = 'Время'; */

        $attr['quest_visible']['Type']    = 'checkbox';
        $attr['quest_visible']['Text']    = 'Видим для пользователя';
        $attr['quest_visible']['Caption'] = 'Видимость';
        $attr['quest_visible']['Label']   = 'Публиковать';

        $attr['quest_name']['Type']  = 'text';
        $attr['quest_name']['Label'] = 'Ф.И.О. Автора';
        $attr['quest_name']['Attr']  = array('size' => 35);

        $attr['quest_mail']['Type']  = 'text';
        $attr['quest_mail']['Label'] = 'E-mail Автора';
        $attr['quest_mail']['Attr']  = array('size' => 35);

        $attr['quest_question']['Type']  = 'textarea';
        $attr['quest_question']['Label'] = 'Вопрос';
        $attr['quest_question']['Attr']  = array('rows'                             => 6, 'cols'                             => 39);
        $attr['quest_question']['Caption'] = 'Вопрос';


        // $attr['quest_file']['Type']  = 'doc';
        // $attr['quest_file']['Label'] = 'Файл-вложение';
        // $attr['quest_file']['Attr']  = array('size' => 35);

        $attr['quest_send']['Type']  = 'checkbox';
        $attr['quest_send']['Label'] = 'Отправка ответа';
        $attr['quest_send']['Text']  = 'Отправить ответ пользователю?';

        $attr['quest_dateq']['Type']  = 'date';
        $attr['quest_dateq']['Label'] = 'Дата ответа';
        $attr['quest_dateq']['Attr']  = array('size' => 32);

        $attr['quest_nameq']['Type']  = 'text';
        $attr['quest_nameq']['Label'] = 'Ф.И.О. Автора ответа';
        $attr['quest_nameq']['Attr']  = array('size' => 35);

        $attr['quest_comment']['Type']  = 'editor';
        $attr['quest_comment']['Label'] = 'Ответ';
        $attr['quest_comment']['Attr']  = array('width'                           => '100%', 'height'                          => '400px');
        $attr['quest_comment']['Caption'] = 'Ответ';

        $attr['part_id']['Type']  = 'hidden';
        $attr['part_id']['Label'] = '';
        return $attr;
    }

}

class Goods extends DB_Table_Object {

    function Goods(&$db, $where) {
        parent::DB_Table_Object($db, 'ref_goods', 'goods_id', 'goods_order', $where);
        //$this->setFormTemplate('templates/admin_photos_form.tpl');
        $this->setCaption("Продукция", "images/catalog.png");
    }

    function &getFormFields() {
        $attr = array();

        $attr['goods_image']['Type']    = 'image';
        $attr['goods_image']['Label']   = 'Картинка';
        $attr['goods_image']['Caption'] = 'Картинка';

        $attr['goods_name']['Type']  = 'text';
        $attr['goods_name']['Label'] = 'Название';
        $attr['goods_name']['Attr']  = array('size'                         => 35);
        $attr['goods_name']['Caption'] = 'Название';

        $attr['goods_content']['Type']  = 'editor';
        $attr['goods_content']['Label'] = 'Содержание';
        $attr['goods_content']['Attr']  = array('width'  => '100%', 'height' => '400px');

        $attr['part_id']['Type']  = 'hidden';
        $attr['part_id']['Label'] = '';
        return $attr;
    }

}

class News extends DB_Table_Object {

    function News(&$db, $where) {
        parent::DB_Table_Object($db, 'ref_news', 'news_id', 'news_order', $where);
        //$this->setFormTemplate('templates/admin_photos_form.tpl');
        $this->setCaption("Новости", "images/news.png");
    }

    function insert($values) {

        $news_date = $values['news_date'];
        if (preg_match('/(\d{1,2})-(\d{1,2})-(\d{4})/i', $values['news_date'], $m)) {
            $values['news_date'] = $m[3] . "-" . $m[2] . "-" . $m[1];
        } else {
            $values['news_date'] = date("Y-m-d");
        }

        return parent::insert($values);
    }

    function update($values, $oid) {
        if (preg_match('/(\d{1,2})-(\d{1,2})-(\d{4})/i', $values['news_date'], $m)) {
            $values['news_date'] = $m[3] . "-" . $m[2] . "-" . $m[1];
        } else {
            $values['news_date'] = date("Y-m-d");
        }
        return parent::update($values, $oid);
    }

    function &getFormFields() {
        $attr = array();

        $attr['news_date']['Type']  = 'date';
        $attr['news_date']['Label'] = 'Дата';
        $attr['news_date']['Attr']  = array('size'                        => 32);
        $attr['news_date']['Caption'] = 'Дата';

        $attr['news_image']['Type']  = 'image';
        $attr['news_image']['Label'] = 'Картинка';


        $attr['news_image_desc']['Type']  = 'text';
        $attr['news_image_desc']['Label'] = 'Описание картинки';
        $attr['news_image_desc']['Attr']  = array('size' => 35);

        $attr['news_resume']['Type']  = 'textarea';
        $attr['news_resume']['Label'] = 'Резюме.';
        $attr['news_resume']['Attr']  = array('rows'                          => 6, 'cols'                          => 39);
        $attr['news_resume']['Caption'] = 'Резюме';

        $attr['news_content']['Type']  = 'editor';
        $attr['news_content']['Label'] = 'Описание';
        $attr['news_content']['Attr']  = array('width'  => '100%', 'height' => '400px');

        $attr['part_id']['Type']  = 'hidden';
        $attr['part_id']['Label'] = '';
        return $attr;
    }

}

class Conference extends DB_Table_Object {

    function Conference(&$db, $where) {
        parent::DB_Table_Object($db, 'ref_conference', 'conference_id', 'conference_order', $where);
        //$this->setFormTemplate('templates/admin_photos_form.tpl');
        $this->setCaption("Конференции", "images/news.png");
    }

    function insert($values) {

        $news_date = $values['conference_date'];
        if (preg_match('/(\d{1,2})-(\d{1,2})-(\d{4})/i', $values['conference_date'], $m)) {
            $values['conference_date'] = $m[3] . "-" . $m[2] . "-" . $m[1];
        } else {
            $values['conference_date'] = date("Y-m-d");
        }

        $news_date = $values['conference_date_end'];
        if (preg_match('/(\d{1,2})-(\d{1,2})-(\d{4})/i', $values['conference_date_end'], $m)) {
            $values['conference_date_end'] = $m[3] . "-" . $m[2] . "-" . $m[1];
        } else {
            $values['conference_date_end'] = date("Y-m-d");
        }

        return parent::insert($values);
    }

    function update($values, $oid) {
        if (preg_match('/(\d{1,2})-(\d{1,2})-(\d{4})/i', $values['conference_date'], $m)) {
            $values['conference_date'] = $m[3] . "-" . $m[2] . "-" . $m[1];
        } else {
            $values['conference_date'] = date("Y-m-d");
        }

        $news_date = $values['conference_date_end'];
        if (preg_match('/(\d{1,2})-(\d{1,2})-(\d{4})/i', $values['conference_date_end'], $m)) {
            $values['conference_date_end'] = $m[3] . "-" . $m[2] . "-" . $m[1];
        } else {
            $values['conference_date_end'] = date("Y-m-d");
        }

        return parent::update($values, $oid);
    }

    function &getFormFields() {
        $attr = array();

        $attr['conference_date']['Type']  = 'date';
        $attr['conference_date']['Label'] = 'Дата начала';
        $attr['conference_date']['Attr']  = array('size'                              => 32);
        $attr['conference_date']['Caption'] = 'Дата начала';

        $attr['conference_date_end']['Type']  = 'date';
        $attr['conference_date_end']['Label'] = 'Дата окончания';
        $attr['conference_date_end']['Attr']  = array('size'                                  => 32);
        $attr['conference_date_end']['Caption'] = 'Дата окончания';


        $attr['conference_image']['Type']  = 'image';
        $attr['conference_image']['Label'] = 'Картинка';


        $attr['conference_image_desc']['Type']    = 'text';
        $attr['conference_image_desc']['Label']   = 'Название конференции';
        $attr['conference_image_desc']['Caption'] = 'Название конференции';


        $attr['conference_resume']['Type']  = 'textarea';
        $attr['conference_resume']['Label'] = 'Резюме';
        $attr['conference_resume']['Attr']  = array('rows' => 6, 'cols' => 39);
        // $attr['conference_resume']['Caption'] = 'Резюме';

        $attr['conference_request']['Type']  = 'checkbox';
        $attr['conference_request']['Text']  = 'Выводить форму заявки на участие в конференции';
        $attr['conference_request']['Label'] = 'Выводить форму заявки на участие в конференции';

        $attr['conference_content']['Type']  = 'editor';
        $attr['conference_content']['Label'] = 'Описание';
        $attr['conference_content']['Attr']  = array('width'  => '100%', 'height' => '400px');

        $attr['conference_name_1']['Type']  = 'text';
        $attr['conference_name_1']['Label'] = 'Название дополнительной страницы 1';
        $attr['conference_name_1']['Attr']  = 'Название дополнительной страницы 1';

        $attr['conference_content_1']['Type']  = 'editor';
        $attr['conference_content_1']['Label'] = 'Описание 1';
        $attr['conference_content_1']['Attr']  = array('width'  => '100%', 'height' => '400px');

        $attr['conference_name_2']['Type']  = 'text';
        $attr['conference_name_2']['Label'] = 'Название дополнительной страницы 2';
        $attr['conference_name_2']['Attr']  = 'Название дополнительной страницы 2';

        $attr['conference_content_2']['Type']  = 'editor';
        $attr['conference_content_2']['Label'] = 'Описание 2';
        $attr['conference_content_2']['Attr']  = array('width'  => '100%', 'height' => '400px');

        $attr['conference_name_3']['Type']  = 'text';
        $attr['conference_name_3']['Label'] = 'Название дополнительной страницы 3';
        $attr['conference_name_3']['Attr']  = 'Название дополнительной страницы 3';

        $attr['conference_content_3']['Type']  = 'editor';
        $attr['conference_content_3']['Label'] = 'Описание 3';
        $attr['conference_content_3']['Attr']  = array('width'  => '100%', 'height' => '400px');

        $attr['conference_name_4']['Type']  = 'text';
        $attr['conference_name_4']['Label'] = 'Название дополнительной страницы 4';
        $attr['conference_name_4']['Attr']  = 'Название дополнительной страницы 4';

        $attr['conference_content_4']['Type']  = 'editor';
        $attr['conference_content_4']['Label'] = 'Описание 4';
        $attr['conference_content_4']['Attr']  = array('width'  => '100%', 'height' => '400px');

        $attr['conference_name_5']['Type']  = 'text';
        $attr['conference_name_5']['Label'] = 'Название дополнительной страницы 5';
        $attr['conference_name_5']['Attr']  = 'Название дополнительной страницы 5';

        $attr['conference_content_5']['Type']  = 'editor';
        $attr['conference_content_5']['Label'] = 'Описание 5';
        $attr['conference_content_5']['Attr']  = array('width'  => '100%', 'height' => '400px');




        $attr['part_id']['Type']  = 'hidden';
        $attr['part_id']['Label'] = '';
        return $attr;
    }

}

class Photos extends DB_Table_Object {

    function Photos(&$db, $where) {
        parent::DB_Table_Object($db, 'ref_photos', 'photo_id', 'photo_order', $where);
        //$this->setFormTemplate('templates/admin_photos_form.tpl');
        $this->setCaption("Фотоальбом", "images/photos.png");
    }

    function &getFormFields() {
        $attr = array();

        $attr['photo_file']['Type']  = 'image';
        $attr['photo_file']['Label'] = 'Фотография';
        $attr['photo_file']['Attr']  = array('size'                         => 35);
        $attr['photo_file']['Caption'] = 'Фотография';

        /*
          $attr['photo_file_big']['Type']  = 'image';
          $attr['photo_file_big']['Label'] = 'Качественная большая фотография';
          $attr['photo_file_big']['Attr']  = array('size' => 35);

          $attr['doc_file']['Type']  = 'doc';
          $attr['doc_file']['Label'] = 'file';
          $attr['doc_file']['Attr']  = array('size' => 35);
          //$attr['photo_file']['Caption'] = 'file'; */

        $attr['photo_name']['Type']  = 'text';
        $attr['photo_name']['Label'] = 'Название фотографии';
        $attr['photo_name']['Attr']  = array('size'                         => 35);
        $attr['photo_name']['Caption'] = 'Название фотографии';

        $attr['photo_comment']['Type']  = 'textarea';
        $attr['photo_comment']['Label'] = 'Описание фотографии';
        $attr['photo_comment']['Attr']  = array('rows' => 6, 'cols' => 39);
        //$attr['photo_comment']['Caption'] = 'Описание фотографии';

        $attr['photo_date']['Type']  = 'date';
        $attr['photo_date']['Label'] = 'Дата внесения';
        $attr['photo_date']['Attr']  = array('size'                         => 32);
        $attr['photo_date']['Caption'] = 'Дата внесения';

        $attr['photo_author']['Type']  = 'text';
        $attr['photo_author']['Label'] = 'Автор фотографии';
        $attr['photo_author']['Attr']  = array('size'                           => 35);
        $attr['photo_author']['Caption'] = 'Автор';

        $attr['part_id']['Type']  = 'hidden';
        $attr['part_id']['Label'] = '';



        return $attr;
    }

    function insert($values) {
        if (preg_match('/(\d{1,2})-(\d{1,2})-(\d{4})/i', $values['photo_date'], $m)) {
            $values['photo_date'] = $m[3] . "-" . $m[2] . "-" . $m[1];
        } else {
            $values['photo_date'] = date("Y-m-d");
        }
        return parent::insert($values);
    }

    function update($values, $oid) {
        if (preg_match('/(\d{1,2})-(\d{1,2})-(\d{4})/i', $values['photo_date'], $m)) {
            $values['photo_date'] = $m[3] . "-" . $m[2] . "-" . $m[1];
        } else {
            $values['photo_date'] = date("Y-m-d");
        }
        return parent::update($values, $oid);
    }

}

class Cat extends DB_Table_Object {

    function Cat(&$db, $where) {
        parent::DB_Table_Object($db, 'ref_catalog', 'cat_id', 'cat_order', $where);
        //$this->setFormTemplate('templates/admin_photos_form.tpl');
        $this->setCaption("Виды работ", "images/catalog.png");
    }

    function &getFormFields() {
        $attr = array();



        $attr['cat_name']['Type']  = 'text';
        $attr['cat_name']['Label'] = 'Наименование';
        $attr['cat_name']['Attr']  = array('size'                       => 35);
        $attr['cat_name']['Caption'] = 'Наименование';

        $attr['cat_mail']['Type']  = 'text';
        $attr['cat_mail']['Label'] = 'E-Mail';
        $attr['cat_mail']['Attr']  = array('size'                       => 35);
        $attr['cat_mail']['Caption'] = 'E-Mail';

        $attr['cat_cont']['Type']  = 'text';
        $attr['cat_cont']['Label'] = 'Контактная информация';
        $attr['cat_cont']['Attr']  = array('size'                       => 35);
        $attr['cat_cont']['Caption'] = 'Контактная информация';


        $attr['cat_resume']['Type']  = 'textarea';
        $attr['cat_resume']['Label'] = 'Краткое описание';
        $attr['cat_resume']['Attr']  = array('rows' => 6, 'cols' => 39);




        $attr['cat_descr']['Type']  = 'editor';
        $attr['cat_descr']['Label'] = 'Полное описание ';
        $attr['cat_descr']['Attr']  = array('width'  => '100%', 'height' => '400px');


        $attr['cat_doc1']['Type']    = 'text';
        $attr['cat_dname1']['Label'] = 'Описание файла-вложения №1';
        $attr['cat_dname1']['Attr']  = array('size'                     => 35);
        $attr['cat_doc1']['Type']  = 'doc';
        $attr['cat_doc1']['Label'] = 'Файл-вложение №1';
        $attr['cat_doc1']['Attr']  = array('size' => 35);



        $attr['cat_doc2']['Type']    = 'text';
        $attr['cat_dname2']['Label'] = 'Описание файла-вложения №2';
        $attr['cat_dname2']['Attr']  = array('size'                     => 35);
        $attr['cat_doc2']['Type']  = 'doc';
        $attr['cat_doc2']['Label'] = 'Файл-вложение №2';
        $attr['cat_doc2']['Attr']  = array('size' => 35);


        $attr['cat_doc3']['Type']    = 'text';
        $attr['cat_dname3']['Label'] = 'Описание файла-вложения №3';
        $attr['cat_dname3']['Attr']  = array('size'                     => 35);
        $attr['cat_doc3']['Type']  = 'doc';
        $attr['cat_doc3']['Label'] = 'Файл-вложение №3';
        $attr['cat_doc3']['Attr']  = array('size' => 35);

        $attr['cat_doc4']['Type']    = 'text';
        $attr['cat_dname4']['Label'] = 'Описание файла-вложения №4';
        $attr['cat_dname4']['Attr']  = array('size'                     => 35);
        $attr['cat_doc4']['Type']  = 'doc';
        $attr['cat_doc4']['Label'] = 'Файл-вложение №4';
        $attr['cat_doc4']['Attr']  = array('size' => 35);


        $attr['cat_doc5']['Type']    = 'text';
        $attr['cat_dname5']['Label'] = 'Описание файла-вложения №5';
        $attr['cat_dname5']['Attr']  = array('size'                     => 35);
        $attr['cat_doc5']['Type']  = 'doc';
        $attr['cat_doc5']['Label'] = 'Файл-вложение №5';
        $attr['cat_doc5']['Attr']  = array('size' => 35);

        $attr['cat_doc6']['Type']    = 'text';
        $attr['cat_dname6']['Label'] = 'Описание файла-вложения №6';
        $attr['cat_dname6']['Attr']  = array('size'                     => 35);
        $attr['cat_doc6']['Type']  = 'doc';
        $attr['cat_doc6']['Label'] = 'Файл-вложение №6';
        $attr['cat_doc6']['Attr']  = array('size' => 35);

        $attr['cat_doc7']['Type']    = 'text';
        $attr['cat_dname7']['Label'] = 'Описание файла-вложения №7';
        $attr['cat_dname7']['Attr']  = array('size'                     => 35);
        $attr['cat_doc7']['Type']  = 'doc';
        $attr['cat_doc7']['Label'] = 'Файл-вложение №7';
        $attr['cat_doc7']['Attr']  = array('size' => 35);




        $attr['cat_doc8']['Type']    = 'text';
        $attr['cat_dname8']['Label'] = 'Описание файла-вложения №8';
        $attr['cat_dname8']['Attr']  = array('size'                     => 35);
        $attr['cat_doc8']['Type']  = 'doc';
        $attr['cat_doc8']['Label'] = 'Файл-вложение №8';
        $attr['cat_doc8']['Attr']  = array('size' => 35);

        $attr['cat_doc9']['Type']    = 'text';
        $attr['cat_dname9']['Label'] = 'Описание файла-вложения №9';
        $attr['cat_dname9']['Attr']  = array('size'                     => 35);
        $attr['cat_doc9']['Type']  = 'doc';
        $attr['cat_doc9']['Label'] = 'Файл-вложение №9';
        $attr['cat_doc9']['Attr']  = array('size' => 35);

        $attr['cat_doc10']['Type']    = 'text';
        $attr['cat_dname10']['Label'] = 'Описание файла-вложения №10';
        $attr['cat_dname10']['Attr']  = array('size'                      => 35);
        $attr['cat_doc10']['Type']  = 'doc';
        $attr['cat_doc10']['Label'] = 'Файл-вложение №10';
        $attr['cat_doc10']['Attr']  = array('size' => 35);



        $attr['part_id']['Type']  = 'hidden';
        $attr['part_id']['Label'] = '';

        return $attr;
    }

}

class Municipal_orders extends DB_Table_Object {

    function Municipal_orders(&$db, $where) {
        parent::DB_Table_Object($db, 'ref_municipal_orders', 'mo_id', 'mo_order', $where);
        //$this->setFormTemplate('templates/admin_photos_form.tpl');
        $this->setCaption("Муниципальные заказы", "images/catalog.png");
    }

    function &getFormFields() {
        $attr = array();

        $attr['pattern_id']['Type']  = 'select';
        $attr['pattern_id']['Label'] = 'Состояние заказа';
        $attr['pattern_id']['Attr']  = array('style'                        => 'width:230px');
        $attr['pattern_id']['DB']      = 'ref_municipal_structures';
        $attr['pattern_id']['Caption'] = 'Состояние заказа';

        $attr['mo_number']['Type']  = 'text';
        $attr['mo_number']['Label'] = 'Номер заказа';
        $attr['mo_number']['Attr']  = array('size'                        => 35);
        $attr['mo_number']['Caption'] = 'Номер заказа';

        $attr['mo_curent_order']['Type']  = 'text';
        $attr['mo_curent_order']['Label'] = 'Наименование заказа';
        $attr['mo_curent_order']['Attr']  = array('size'                              => 35);
        $attr['mo_curent_order']['Caption'] = 'Наименование заказа';

        $attr['mo_curent_orders']['Type']  = 'text';
        $attr['mo_curent_orders']['Label'] = 'Заказчик';
        $attr['mo_curent_orders']['Attr']  = array('size'                               => 35);
        $attr['mo_curent_orders']['Caption'] = 'Заказчик';

        $attr['mo_curent_prices']['Type']  = 'text';
        $attr['mo_curent_prices']['Label'] = 'Цена';
        $attr['mo_curent_prices']['Attr']  = array('size' => 35);
        //$attr['mo_curent_prices']['Caption'] = 'Цена';

        $attr['mo_start_date']['Type']  = 'date';
        $attr['mo_start_date']['Label'] = 'Дата начала приема заявок';
        $attr['mo_start_date']['Attr']  = array('size'                            => 32);
        $attr['mo_start_date']['Caption'] = 'Дата начала приема заявок';

        $attr['mo_start_time']['Type']  = 'text';
        $attr['mo_start_time']['Label'] = 'Время начала приема заявок';
        $attr['mo_start_time']['Attr']  = array('size'                            => 35);
        $attr['mo_start_time']['Caption'] = 'Время начала приема заявок';

        $attr['mo_start_merch_date']['Type']  = 'date';
        $attr['mo_start_merch_date']['Label'] = 'Дата начала торгов';
        $attr['mo_start_merch_date']['Attr']  = array('size'                                  => 32);
        $attr['mo_start_merch_date']['Caption'] = 'Дата начала торгов';

        $attr['mo_start_merch_time']['Type']  = 'text';
        $attr['mo_start_merch_time']['Label'] = 'Время начала торгов';
        $attr['mo_start_merch_time']['Attr']  = array('size'                                  => 35);
        $attr['mo_start_merch_time']['Caption'] = 'Время начала торгов';

        $attr['mo_contacts']['Type']  = 'text';
        $attr['mo_contacts']['Label'] = 'Контактные данные ';
        $attr['mo_contacts']['Attr']  = array('size' => 35);
        //$attr['mo_contacts']['Caption'] = 'Контактные данные ';

        $attr['mo_file0_name']['Type']  = 'text';
        $attr['mo_file0_name']['Label'] = 'Описание файла-вложения №1';
        $attr['mo_file0_name']['Attr']  = array('size'                     => 35);
        $attr['mo_file0']['Type']  = 'doc';
        $attr['mo_file0']['Label'] = 'Файл-вложение №1';
        $attr['mo_file0']['Attr']  = array('size' => 35);

        $attr['mo_file1_name']['Type']  = 'text';
        $attr['mo_file1_name']['Label'] = 'Описание файла-вложения №2';
        $attr['mo_file1_name']['Attr']  = array('size'                     => 35);
        $attr['mo_file1']['Type']  = 'doc';
        $attr['mo_file1']['Label'] = 'Файл-вложение №2';
        $attr['mo_file1']['Attr']  = array('size' => 35);

        $attr['mo_file2_name']['Type']  = 'text';
        $attr['mo_file2_name']['Label'] = 'Описание файла-вложения №3';
        $attr['mo_file2_name']['Attr']  = array('size'                     => 35);
        $attr['mo_file2']['Type']  = 'doc';
        $attr['mo_file2']['Label'] = 'Файл-вложение №3';
        $attr['mo_file2']['Attr']  = array('size' => 35);

        $attr['mo_file3_name']['Type']  = 'text';
        $attr['mo_file3_name']['Label'] = 'Описание файла-вложения №4';
        $attr['mo_file3_name']['Attr']  = array('size'                     => 35);
        $attr['mo_file3']['Type']  = 'doc';
        $attr['mo_file3']['Label'] = 'Файл-вложение №4';
        $attr['mo_file3']['Attr']  = array('size' => 35);

        $attr['mo_file4_name']['Type']  = 'text';
        $attr['mo_file4_name']['Label'] = 'Описание файла-вложения №5';
        $attr['mo_file4_name']['Attr']  = array('size'                     => 35);
        $attr['mo_file4']['Type']  = 'doc';
        $attr['mo_file4']['Label'] = 'Файл-вложение №5';
        $attr['mo_file4']['Attr']  = array('size' => 35);

        $attr['mo_file5_name']['Type']  = 'text';
        $attr['mo_file5_name']['Label'] = 'Описание файла-вложения №6';
        $attr['mo_file5_name']['Attr']  = array('size'                     => 35);
        $attr['mo_file5']['Type']  = 'doc';
        $attr['mo_file5']['Label'] = 'Файл-вложение №6';
        $attr['mo_file5']['Attr']  = array('size' => 35);

        $attr['mo_file6_name']['Type']  = 'text';
        $attr['mo_file6_name']['Label'] = 'Описание файла-вложения №7';
        $attr['mo_file6_name']['Attr']  = array('size'                     => 35);
        $attr['mo_file6']['Type']  = 'doc';
        $attr['mo_file6']['Label'] = 'Файл-вложение №7';
        $attr['mo_file6']['Attr']  = array('size' => 35);

        $attr['mo_file7_name']['Type']  = 'text';
        $attr['mo_file7_name']['Label'] = 'Описание файла-вложения №8';
        $attr['mo_file7_name']['Attr']  = array('size'                     => 35);
        $attr['mo_file7']['Type']  = 'doc';
        $attr['mo_file7']['Label'] = 'Файл-вложение №8';
        $attr['mo_file7']['Attr']  = array('size' => 35);

        $attr['mo_file8_name']['Type']  = 'text';
        $attr['mo_file8_name']['Label'] = 'Описание файла-вложения №9';
        $attr['mo_file8_name']['Attr']  = array('size'                     => 35);
        $attr['mo_file8']['Type']  = 'doc';
        $attr['mo_file8']['Label'] = 'Файл-вложение №9';
        $attr['mo_file8']['Attr']  = array('size' => 35);

        $attr['mo_file9_name']['Type']  = 'text';
        $attr['mo_file9_name']['Label'] = 'Описание файла-вложения №10';
        $attr['mo_file9_name']['Attr']  = array('size'                     => 35);
        $attr['mo_file9']['Type']  = 'doc';
        $attr['mo_file9']['Label'] = 'Файл-вложение №10';
        $attr['mo_file9']['Attr']  = array('size' => 35);

        $attr['part_id']['Type']  = 'hidden';
        $attr['part_id']['Label'] = '';

        return $attr;
    }

    function insert($values) {
        if (preg_match('/(\d{1,2})-(\d{1,2})-(\d{4})/i', $values['mo_start_date'], $m)) {
            $values['mo_start_date'] = $m[3] . "-" . $m[2] . "-" . $m[1];
        } else {
            $values['mo_start_date'] = date("Y-m-d");
        }

        if (preg_match('/(\d{1,2})-(\d{1,2})-(\d{4})/i', $values['mo_start_merch_date'], $m)) {
            $values['mo_start_merch_date'] = $m[3] . "-" . $m[2] . "-" . $m[1];
        } else {
            $values['mo_start_merch_date'] = date("Y-m-d");
        }

        return parent::insert($values);
    }

    function update($values, $oid) {
        if (preg_match('/(\d{1,2})-(\d{1,2})-(\d{4})/i', $values['mo_start_date'], $m)) {
            $values['mo_start_date'] = $m[3] . "-" . $m[2] . "-" . $m[1];
        } else {
            $values['mo_start_date'] = date("Y-m-d");
        }

        if (preg_match('/(\d{1,2})-(\d{1,2})-(\d{4})/i', $values['mo_start_merch_date'], $m)) {
            $values['mo_start_merch_date'] = $m[3] . "-" . $m[2] . "-" . $m[1];
        } else {
            $values['mo_start_merch_date'] = date("Y-m-d");
        }
        return parent::update($values, $oid);
    }

}

class User extends DB_Table_Object {

    function User(&$db) {
        @require_once("admin_permission.php");
        parent::DB_Table_Object($db, 'ref_users', 'user_id', '');
        $this->setFormTemplate('templates/admin_user_form.tpl');
    }

    function &getFormFields() {
        $attr = array();

        $attr['user_name']['Type']  = 'text';
        $attr['user_name']['Label'] = 'ФИО';
        $attr['user_name']['Attr']  = array('size'                        => 35);
        $attr['user_name']['Caption'] = 'ФИО';

        $attr['user_login']['Type']  = 'text';
        $attr['user_login']['Label'] = 'Логин';
        $attr['user_login']['Attr']  = array('size'                         => 35);
        $attr['user_login']['Caption'] = 'Логин';

        $attr['user_type_id']['Type']    = 'select';
        $attr['user_type_id']['Label']   = 'Тип пользователя';
        $attr['user_type_id']['DB']      = 'ref_user_type';
        $attr['user_type_id']['Caption'] = 'Тип';
        $attr['user_type_id']['Attr']    = array('style'    => 'width:230px', 'onchange' => 'changeUserType(this)');


        $attr['user_password']['Type']  = 'password';
        $attr['user_password']['Label'] = 'Пароль';
        $attr['user_password']['Attr']  = array('size' => 35);


        $attr['user_password_confirm']['Type']  = 'password';
        $attr['user_password_confirm']['Label'] = 'Подтвердить пароль';
        $attr['user_password_confirm']['Attr']  = array('size' => 35);


        $attr['user_email']['Type']  = 'text';
        $attr['user_email']['Label'] = 'E-mail';
        $attr['user_email']['Attr']  = array('size'                         => 35);
        $attr['user_email']['Caption'] = 'E-mail';

        $attr['user_date']['Type']  = 'date';
        $attr['user_date']['Label'] = 'Дата создания';
        $attr['user_date']['Attr']  = array('size'                        => 32);
        $attr['user_date']['Caption'] = 'Дата создания';

        $attr['user_ip']['Type']  = 'text';
        $attr['user_ip']['Label'] = 'IP';
        $attr['user_ip']['Attr']  = array('size'                      => 35);
        $attr['user_ip']['Caption'] = 'IP';


        $attr['user_permission']['Type']  = 'new';
        $attr['user_permission']['Label'] = 'Доступ к разделам';
        $attr['user_permission']['Attr']  = array('size'                            => 32);
        $attr['user_permission']['Class'] = 'PermissionPanel';

        $attr['user_permit_hidden']['Type']  = 'hidden';
        $attr['user_permit_hidden']['Label'] = '';
        $attr['user_permit_hidden']['Attr']  = array('size' => 32);

        return $attr;
    }

    function insert($values) {

        $sql   = "SELECT *FROM $this->_tbl WHERE user_login='$values[user_login]'";
        $match = $this->_db->getRow($sql, array(), DB_FETCHMODE_ORDERED);
        if (!is_null($match)) {
            $_SESSION['form']    = $values;
            $_SESSION['message'] = 'Пользователь с данным логином уже существует...';
            $currUrl             = $_SERVER['REQUEST_URI'];

            if (preg_match('/permit=([^&]*)/i', $currUrl, $m)) {
                $currUrl = preg_replace('/' . $m[0] . '/i', 'permit=' . $values['user_permit_hidden'], $currUrl);
            }

            header('Location:' . $currUrl);
            exit();
        }
        $array        = explode('@', $values['user_permit_hidden']);
        $permitString = $array[0];
        unset($values['user_password_confirm']);
        unset($values['user_permit_hidden']);
        if (preg_match('/(\d{1,2})-(\d{1,2})-(\d{4})/i', $values['user_date'], $m)) {
            $values['user_date'] = $m[3] . "-" . $m[2] . "-" . $m[1];
        } else {
            $values['user_date'] = date("Y-m-d");
        }
        // if ID is empty then set default IP
        $values['user_ip']   = empty($values['user_ip']) ? getenv('REMOTE_ADDR') : $values['user_ip'];
        // insert user
        parent::insert($values);
        if ((int) $values['user_type_id'] > 1) {

            $sql = "SELECT $this->_tbl_key FROM $this->_tbl ORDER BY $this->_tbl_key DESC";
            $row = $this->_db->getRow($sql, array(), DB_FETCHMODE_ASSOC);
            $permits = explode(',', $permitString);
            $values  = array();
            $values[$this->_tbl_key] = $row[$this->_tbl_key];
            foreach ($permits as $permit) {
                $values['part_id'] = $permit;
                if ((int) $values['part_id'] > 0) {
                    insert($this->_db, 'ref_user_permissions', $values);
                }
            }
        }
    }

    function update($values, $oid) {
        $sql   = "SELECT *FROM $this->_tbl WHERE user_login='$values[user_login]'
		        AND $this->_tbl_key!=$oid";
        $match = $this->_db->getRow($sql, array(), DB_FETCHMODE_ORDERED);
        if (!is_null($match)) {
            $_SESSION['form']    = $values;
            $_SESSION['message'] = 'Пользователь с данным логином уже существует...';
            $currUrl             = $_SERVER['REQUEST_URI'];

            if (preg_match('/permit=([^&]*)/i', $currUrl, $m)) {
                $currUrl = preg_replace('/' . $m[0] . '/i', 'permit=' . $values['user_permit_hidden'], $currUrl);
            }

            header('Location:' . $currUrl);
            exit();
        }
        $array        = explode('@', $values['user_permit_hidden']);
        $permitString = $array[0];

        unset($values['user_password_confirm']);
        unset($values['user_permit_hidden']);
        if (preg_match('/(\d{1,2})-(\d{1,2})-(\d{4})/i', $values['user_date'], $m)) {
            $values['user_date'] = $m[3] . "-" . $m[2] . "-" . $m[1];
        } else {
            $values['user_date'] = date("Y-m-d");
        }
        // delete old permissions
        delete($this->_db, 'ref_user_permissions', 'user_id', $oid);
        // write new permissions
        if ((int) $values['user_type_id'] > 1) {
            // insert permission
            $sql = "SELECT $this->_tbl_key FROM $this->_tbl ORDER BY $this->_tbl_key DESC";
            $row = $this->_db->getRow($sql, array(), DB_FETCHMODE_ASSOC);
            $permits = explode(',', $permitString);
            $vals    = array();
            $vals[$this->_tbl_key] = $oid;
            foreach ($permits as $permit) {
                $vals['part_id'] = $permit;
                if ((int) $vals['part_id'] > 0) {
                    insert($this->_db, 'ref_user_permissions', $vals);
                }
            }
        }
        // if ID is empty then set default IP
        $values['user_ip'] = empty($values['user_ip']) ? getenv('REMOTE_ADDR') : $values['user_ip'];
        return parent::update($values, $oid);
    }

    function getDetails($oid) {
        $db     = &$this->_db;
        $permit = !isset($_GET['permit']) ? null : $_GET['permit'];
        if (is_null($permit)) {
            $sql  = "SELECT *FROM ref_user_permissions WHERE user_id=" . $oid;
            $rows = &$db->getAll($sql, array(), DB_FETCHMODE_ASSOC);
            $permit = "";
            foreach ($rows as $row) {
                if (!empty($permit)) {
                    $permit .= ",";
                }
                $permit .= $row['part_id'];
            }
            $currUrl = $_SERVER['REQUEST_URI'] . "&permit=$permit";
            header('Location:' . $currUrl);
            exit();
        }
        $sql     = "SELECT *
               FROM " . $this->_tbl . " WHERE " . $this->_tbl_key . " = " . $oid;
        $row     = &$db->getRow($sql, array(), DB_FETCHMODE_ASSOC);
        $row['user_password_confirm'] = $row['user_password'];
        return $row;
    }

    function delete($oid) {
        $sql  = "SELECT *FROM ref_user_permissions WHERE user_id=" . $oid;
        $rows = $this->_db->getAll($sql, array(), DB_FETCHMODE_ASSOC);
        if (!is_null($rows)) {
            foreach ($rows as $row) {
                delete($this->_db, 'ref_user_permissions', 'user_id', $oid);
            }
        }
        return parent::delete($oid);
    }

    function onFormSubmit() {
        $script = "
		var form = document.forms.form;
		if (form.elements['form[user_login]'].value.trim() == '')
		{
		   alert('Поле \"Логин\" заполнено неверно...');
		   form.elements['form[user_login]'].focus();
		   return false;
		}
		if ((form.elements['form[user_password]'].value.trim() == '')||(form.elements['form[user_password]'].value != form.elements['form[user_password_confirm]'].value))
		{
		   form.elements['form[user_password]'].focus();
		   alert('Нет соответствия между полем \"Пароль\" и \"Подтвердить пароль\"');
		   return false;
		}
		if (form.elements['form[user_email]'].value.trim() == '' || !checkEmail(form.elements['form[user_email]']))
		{
		   form.elements['form[user_email]'].focus();
		   alert('Ошибка ввода адреса электронной почты');
		   return false;
		}
		var permits = getPermits()
		form.elements['form[user_permit_hidden]'].value = permits;
		if (permits.indexOf('@') < 3 && form.elements['form[user_type_id]'].value != 1) {
		 alert('Отметьте еще один раздел для доступа к разделам сайта');
		 return false;
		}
		";
        return $script;
    }

}

class UserAdmin extends User {

    var $_cookieName  = 'cookieUser';
    var $_sessionName = 'sessionUser';
    var $_logged      = false;
    var $_request     = null;
    var $_login       = 'undefined';
    var $_type        = 0;
    var $_id          = 0;

    function UserAdmin(&$db) {
        parent::User($db);
        $this->_request = new Request();
        $session = $this->_request->getSESSIONElem($this->_sessionName);
        $this->_logged = $this->_checkSession($session);
        if ($this->registered()) {

            $this->_id = $session['user_id'];
            $this->_login = $session['user_login'];
            $this->_type = $session['user_type'];
        }
    }

    function _checkSession($session) {
        if (!isset($session['user_key']) || !isset($session['user_ip']) || !isset($session['user_login']) || !isset($session['user_date']) || !isset($session['user_password'])) {
            return false;
        }
        return ($session['user_key'] == md5($session['user_ip'] . $session['user_login'] . $session['user_date'] . $session['user_password']));
    }

    function _setSession($values) {
        if (!isset($values['user_ip']) || !isset($values['user_login']) || !isset($values['user_date']) || !isset($values['user_password'])) {
            return false;
        }
        $session = array();
        $session['user_ip']       = $values['user_ip'];
        $session['user_login']    = $values['user_login'];
        $session['user_date']     = $values['user_date'];
        $session['user_password'] = $values['user_password'];
        $session['user_ip']       = $values['user_ip'];
        $session['user_id']       = $values['user_id'];
        $session['user_type']     = $values['user_type_id'];
        $session['user_key']      = md5($session['user_ip'] . $session['user_login'] . $session['user_date'] . $session['user_password']);
        $this->_request->setSESSIONElem($this->_sessionName, $session);
        return true;
    }

    function getPermitsAsArray() {
        $array = array();
        $sql = "SELECT part_id, user_id FROM ref_user_permissions WHERE user_id=$this->_id";
        $res = $this->_db->getAll($sql, array(), DB_FETCHMODE_ASSOC);
        foreach ($res as $row) {
            $array[] = $row['part_id'];
        }
        return $array;
    }

    function getPermitsAsString($sep = ",") {
        return implode($sep, $this->getPermitsAsArray());
    }

    function registered() {
        return $this->_logged;
    }

    function checkLogin($login, $password) {
        $sql = "SELECT * FROM $this->_tbl WHERE " .
                "(user_login    = '$login') AND " .
                "(user_password = '" . $password . "')";
        $res = &$this->_db->getRow($sql, array(), DB_FETCHMODE_ASSOC);

        return (is_array($res) && $this->_setSession($res));
    }

    function logout() {
        $session = $this->_request->getSESSIONElem($this->_sessionName);
        $this->_request->removeSESSIONElem($this->_sessionName);
        $this->_logged = false;
    }

    function exist($login) {
        $db  = &$this->_db;
        $sql = "SELECT *FROM $this->_tbl WHERE user_login = '$login' ";
        $res = &$this->_db->getRow($sql, array(), DB_FETCHMODE_ASSOC);
        return is_array($res);
    }

    function getType() {
        return $this->_type;
    }

    function getLogin() {
        return $this->_login;
    }

    function getId() {
        return $this->_id;
    }

}

class Component {

    var $component = null;

    function Component(&$db, $where = "") {
        $type = "";
        if (!empty($_SESSION['comp_type'])) {
            $type = $_SESSION['comp_type'];
        }
        switch ($type) {
            case 'users':
                $this->component = new User($db);
                break;
            case 'goods':
                $this->component = new Goods($db, $where);
                break;

            case 'news':
                $this->component = new News($db, $where);
                break;

            case 'articles':
                $this->component = new Articles($db, $where);
                break;

            case 'conference':
                $this->component = new Conference($db, $where);
                break;

            case 'prices':
                $this->component = new Prices($db, $where);
                break;

            case 'question':
                $this->component = new Quest($db, $where);
                break;
            case 'photos':
                $this->component = new Photos($db, $where);
                break;
            case 'catalog':
                $this->component = new Cat($db, $where);
                break;
            case 'municipal_orders':
                $this->component = new Municipal_orders($db, $where);
                break;
            case 'cena':
                $this->component = new Cena($db, $where);
                break;
            default:
                $this->component = new Parts($db, $where);
                break;
        }

        return $this->component;
    }

}

?>