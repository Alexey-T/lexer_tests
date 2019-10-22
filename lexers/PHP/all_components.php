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
        $attr['part_name']['Label'] = '�������� �������';
        $attr['part_name']['Attr']  = array('size'                        => 35);
        $attr['part_name']['Caption'] = '�������� �������';


        $attr['part_parent_id']['Type']  = 'select';
        $attr['part_parent_id']['Label'] = '������������ ����';
        $attr['part_parent_id']['Attr']  = array('style'                            => 'width:230px');
        $attr['part_parent_id']['Query']   = "SELECT $this->_tbl_key, part_name FROM $this->_tbl";
        $attr['part_parent_id']['Options'] = array(array(0 => '0', 1 => MAIN_NODE));


        $attr['pattern_id']['Type']  = 'select';
        $attr['pattern_id']['Label'] = '��� �������';
        $attr['pattern_id']['Attr']  = array('style'                        => 'width:230px');
        $attr['pattern_id']['DB']      = 'ref_patterns';
        $attr['pattern_id']['Caption'] = '��� �������';

        $attr['structure_id']['Type']  = 'select';
        $attr['structure_id']['Label'] = '������������';
        $attr['structure_id']['Attr']  = array('style'                          => 'width:230px');
        $attr['structure_id']['DB']      = 'ref_structures';
        $attr['structure_id']['Caption'] = '������������';


        $attr['part_visible']['Type']    = 'checkbox';
        $attr['part_visible']['Caption'] = '���������';
        $attr['part_visible']['Text']    = '����� ��� ������������';
        $attr['part_visible']['Label']   = '���������';
        //
        if ($op != 'edit') {
            $attr['part_push']['Type']  = 'select';
            $attr['part_push']['Label'] = '��������';
            $attr['part_push']['Attr']  = array('style'                       => 'width:230px');
            $attr['part_push']['Options'] = array(array(0 => '0', 1 => '������'), array(0 => '1', 1 => '���������'));
        } else {
            $attr['part_push']['Type']      = 'hidden';
            $attr['part_push']['Label']     = '';
        }
        //
        $attr['part_add_info']['Type']  = 'editor';
        $attr['part_add_info']['Label'] = '���������� �������';
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
        $attr['part_image_path']['Label'] = '���� � ���������';
        $attr['part_image_path']['Attr']  = array('style'                              => 'width:230px');
        $attr['part_image_path']['Options']  = $paths;
        $attr['part_image_path']['Selected'] = '../Image';
        // set docs path
        $attr['part_docs_path']['Type']      = 'select';
        $attr['part_docs_path']['Label']     = '���� � ������';
        $attr['part_docs_path']['Attr']      = array('style'                             => 'width:230px');
        $attr['part_docs_path']['Options']  = $paths;
        $attr['part_docs_path']['Selected'] = '../File';
        return $attr;
    }

    function insert($values, $type = 'insert') {
        global $user;

        if ($user->getType() != 1) {
            exit('����������� ��������. �� ��������� �������� ��������������.');
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
            exit('����������� ��������. �� ��������� �������� ��������������.');
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
            exit('����������� ��������. �� ��������� �������� ��������������.');
        }

        $values = array();
        $values['part_visible'] = 1;
        return parent::update($values, $oid, 'publish');
    }

    function unpublish($oid) {
        global $user;
        if ($user->getType() != 1) {
            exit('����������� ��������. �� ��������� �������� ��������������.');
        }

        $values = array();
        $values['part_visible'] = 0;
        return parent::update($values, $oid, 'unpublish');
    }

    function delete($oid) {
        global $user;
        if ($user->getType() != 1) {
            exit('����������� ��������. �� ��������� �������� ��������������.');
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
        $this->setCaption("������", "images/news.png");
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
        $attr['articles_date']['Label'] = '����';
        $attr['articles_date']['Attr']  = array('size'                            => 32);
        $attr['articles_date']['Caption'] = '����';

        $attr['articles_image']['Type']  = 'image';
        $attr['articles_image']['Label'] = '��������';


        $attr['articles_image_desc']['Type']  = 'text';
        $attr['articles_image_desc']['Label'] = '��������';
        $attr['articles_image_desc']['Attr']  = array('size'                                  => 35);
        $attr['articles_image_desc']['Caption'] = '��������';

        $attr['articles_resume']['Type']  = 'textarea';
        $attr['articles_resume']['Label'] = '������';
        $attr['articles_resume']['Attr']  = array('rows' => 6, 'cols' => 39);
//        $attr['articles_resume']['Caption'] = '������';

        $attr['articles_content']['Type']  = 'editor';
        $attr['articles_content']['Label'] = '��������';
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
        $this->setCaption("���������", "images/docs.png");
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
            $attr['price_date']['Label']   = '���� ����������';
            $attr['price_date']['Caption'] = '���� ����������';
            $attr['price_date']['Attr']    = array('size' => 32);
        }

        $attr['price_file']['Type']  = 'doc';
        $attr['price_file']['Label'] = '����-��������';

        $attr['price_name']['Type']  = 'text';
        $attr['price_name']['Label'] = '��������';
        $attr['price_name']['Attr']  = array('size'                         => 35);
        $attr['price_name']['Caption'] = '��������';


        $attr['part_id']['Type']  = 'hidden';
        $attr['part_id']['Label'] = '';
        return $attr;
    }

}

class Cena extends DB_Table_Object {

    function Cena(&$db, $where) {
        parent::DB_Table_Object($db, 'ref_cena', 'cena_id', 'cena_order', $where);
        $this->setCaption("���� �� ���������", "images/photos.png");
    }

    function &getFormFields() {
        $attr = array();
        $attr['cena_tolsh']['Type']  = 'text';
        $attr['cena_tolsh']['Label'] = '������� ���������';
        $attr['cena_tolsh']['Attr']  = array('size'                         => 35);
        $attr['cena_tolsh']['Caption'] = '������� ���������';
        $attr['cena_price']['Type']    = 'text';
        $attr['cena_price']['Label']   = '����';
        $attr['cena_price']['Attr']    = array('size'                         => 35);
        $attr['cena_price']['Caption'] = '����';
        $attr['part_id']['Type']       = 'hidden';
        $attr['part_id']['Label']      = '';
        return $attr;
    }

}

class Quest extends DB_Table_Object {

    function Quest(&$db, $where) {
        parent::DB_Table_Object($db, 'ref_quest', 'quest_id', 'quest_order', $where);
        //$this->setFormTemplate('templates/admin_photos_form.tpl');
        $this->setCaption("������-�����", "images/links.png");
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
				<h4>����� �� ��� ������</h4>
				<br />
				<strong>��� ������:</strong> {$values[quest_question]}<br />
				<strong>�����:</strong> {$values[quest_comment]}<br /><br />
				� ���������,<br />
			";

            mail($values["quest_mail"], "����� �� ������ �� ����� www.estet-center.ru", $body, "From:" . MAIL_TO . "\nContent-Type: text/html; charset=\"Windows-1251\"");
        }
        unset($values['quest_send']);
        return parent::update($values, $oid);
    }

    function &getFormFields() {
        $attr = array();

        $attr['quest_date']['Type']  = 'date';
        $attr['quest_date']['Label'] = '����';
        $attr['quest_date']['Attr']  = array('size'                         => 32);
        $attr['quest_date']['Caption'] = '����';

        /*
          $attr['quest_time']['Type']  = 'time';
          $attr['quest_time']['Label'] = '�����';
          $attr['quest_time']['Attr'] = array('size' => 32);
          $attr['quest_time']['Caption'] = '�����'; */

        $attr['quest_visible']['Type']    = 'checkbox';
        $attr['quest_visible']['Text']    = '����� ��� ������������';
        $attr['quest_visible']['Caption'] = '���������';
        $attr['quest_visible']['Label']   = '�����������';

        $attr['quest_name']['Type']  = 'text';
        $attr['quest_name']['Label'] = '�.�.�. ������';
        $attr['quest_name']['Attr']  = array('size' => 35);

        $attr['quest_mail']['Type']  = 'text';
        $attr['quest_mail']['Label'] = 'E-mail ������';
        $attr['quest_mail']['Attr']  = array('size' => 35);

        $attr['quest_question']['Type']  = 'textarea';
        $attr['quest_question']['Label'] = '������';
        $attr['quest_question']['Attr']  = array('rows'                             => 6, 'cols'                             => 39);
        $attr['quest_question']['Caption'] = '������';


        // $attr['quest_file']['Type']  = 'doc';
        // $attr['quest_file']['Label'] = '����-��������';
        // $attr['quest_file']['Attr']  = array('size' => 35);

        $attr['quest_send']['Type']  = 'checkbox';
        $attr['quest_send']['Label'] = '�������� ������';
        $attr['quest_send']['Text']  = '��������� ����� ������������?';

        $attr['quest_dateq']['Type']  = 'date';
        $attr['quest_dateq']['Label'] = '���� ������';
        $attr['quest_dateq']['Attr']  = array('size' => 32);

        $attr['quest_nameq']['Type']  = 'text';
        $attr['quest_nameq']['Label'] = '�.�.�. ������ ������';
        $attr['quest_nameq']['Attr']  = array('size' => 35);

        $attr['quest_comment']['Type']  = 'editor';
        $attr['quest_comment']['Label'] = '�����';
        $attr['quest_comment']['Attr']  = array('width'                           => '100%', 'height'                          => '400px');
        $attr['quest_comment']['Caption'] = '�����';

        $attr['part_id']['Type']  = 'hidden';
        $attr['part_id']['Label'] = '';
        return $attr;
    }

}

class Goods extends DB_Table_Object {

    function Goods(&$db, $where) {
        parent::DB_Table_Object($db, 'ref_goods', 'goods_id', 'goods_order', $where);
        //$this->setFormTemplate('templates/admin_photos_form.tpl');
        $this->setCaption("���������", "images/catalog.png");
    }

    function &getFormFields() {
        $attr = array();

        $attr['goods_image']['Type']    = 'image';
        $attr['goods_image']['Label']   = '��������';
        $attr['goods_image']['Caption'] = '��������';

        $attr['goods_name']['Type']  = 'text';
        $attr['goods_name']['Label'] = '��������';
        $attr['goods_name']['Attr']  = array('size'                         => 35);
        $attr['goods_name']['Caption'] = '��������';

        $attr['goods_content']['Type']  = 'editor';
        $attr['goods_content']['Label'] = '����������';
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
        $this->setCaption("�������", "images/news.png");
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
        $attr['news_date']['Label'] = '����';
        $attr['news_date']['Attr']  = array('size'                        => 32);
        $attr['news_date']['Caption'] = '����';

        $attr['news_image']['Type']  = 'image';
        $attr['news_image']['Label'] = '��������';


        $attr['news_image_desc']['Type']  = 'text';
        $attr['news_image_desc']['Label'] = '�������� ��������';
        $attr['news_image_desc']['Attr']  = array('size' => 35);

        $attr['news_resume']['Type']  = 'textarea';
        $attr['news_resume']['Label'] = '������.';
        $attr['news_resume']['Attr']  = array('rows'                          => 6, 'cols'                          => 39);
        $attr['news_resume']['Caption'] = '������';

        $attr['news_content']['Type']  = 'editor';
        $attr['news_content']['Label'] = '��������';
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
        $this->setCaption("�����������", "images/news.png");
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
        $attr['conference_date']['Label'] = '���� ������';
        $attr['conference_date']['Attr']  = array('size'                              => 32);
        $attr['conference_date']['Caption'] = '���� ������';

        $attr['conference_date_end']['Type']  = 'date';
        $attr['conference_date_end']['Label'] = '���� ���������';
        $attr['conference_date_end']['Attr']  = array('size'                                  => 32);
        $attr['conference_date_end']['Caption'] = '���� ���������';


        $attr['conference_image']['Type']  = 'image';
        $attr['conference_image']['Label'] = '��������';


        $attr['conference_image_desc']['Type']    = 'text';
        $attr['conference_image_desc']['Label']   = '�������� �����������';
        $attr['conference_image_desc']['Caption'] = '�������� �����������';


        $attr['conference_resume']['Type']  = 'textarea';
        $attr['conference_resume']['Label'] = '������';
        $attr['conference_resume']['Attr']  = array('rows' => 6, 'cols' => 39);
        // $attr['conference_resume']['Caption'] = '������';

        $attr['conference_request']['Type']  = 'checkbox';
        $attr['conference_request']['Text']  = '�������� ����� ������ �� ������� � �����������';
        $attr['conference_request']['Label'] = '�������� ����� ������ �� ������� � �����������';

        $attr['conference_content']['Type']  = 'editor';
        $attr['conference_content']['Label'] = '��������';
        $attr['conference_content']['Attr']  = array('width'  => '100%', 'height' => '400px');

        $attr['conference_name_1']['Type']  = 'text';
        $attr['conference_name_1']['Label'] = '�������� �������������� �������� 1';
        $attr['conference_name_1']['Attr']  = '�������� �������������� �������� 1';

        $attr['conference_content_1']['Type']  = 'editor';
        $attr['conference_content_1']['Label'] = '�������� 1';
        $attr['conference_content_1']['Attr']  = array('width'  => '100%', 'height' => '400px');

        $attr['conference_name_2']['Type']  = 'text';
        $attr['conference_name_2']['Label'] = '�������� �������������� �������� 2';
        $attr['conference_name_2']['Attr']  = '�������� �������������� �������� 2';

        $attr['conference_content_2']['Type']  = 'editor';
        $attr['conference_content_2']['Label'] = '�������� 2';
        $attr['conference_content_2']['Attr']  = array('width'  => '100%', 'height' => '400px');

        $attr['conference_name_3']['Type']  = 'text';
        $attr['conference_name_3']['Label'] = '�������� �������������� �������� 3';
        $attr['conference_name_3']['Attr']  = '�������� �������������� �������� 3';

        $attr['conference_content_3']['Type']  = 'editor';
        $attr['conference_content_3']['Label'] = '�������� 3';
        $attr['conference_content_3']['Attr']  = array('width'  => '100%', 'height' => '400px');

        $attr['conference_name_4']['Type']  = 'text';
        $attr['conference_name_4']['Label'] = '�������� �������������� �������� 4';
        $attr['conference_name_4']['Attr']  = '�������� �������������� �������� 4';

        $attr['conference_content_4']['Type']  = 'editor';
        $attr['conference_content_4']['Label'] = '�������� 4';
        $attr['conference_content_4']['Attr']  = array('width'  => '100%', 'height' => '400px');

        $attr['conference_name_5']['Type']  = 'text';
        $attr['conference_name_5']['Label'] = '�������� �������������� �������� 5';
        $attr['conference_name_5']['Attr']  = '�������� �������������� �������� 5';

        $attr['conference_content_5']['Type']  = 'editor';
        $attr['conference_content_5']['Label'] = '�������� 5';
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
        $this->setCaption("����������", "images/photos.png");
    }

    function &getFormFields() {
        $attr = array();

        $attr['photo_file']['Type']  = 'image';
        $attr['photo_file']['Label'] = '����������';
        $attr['photo_file']['Attr']  = array('size'                         => 35);
        $attr['photo_file']['Caption'] = '����������';

        /*
          $attr['photo_file_big']['Type']  = 'image';
          $attr['photo_file_big']['Label'] = '������������ ������� ����������';
          $attr['photo_file_big']['Attr']  = array('size' => 35);

          $attr['doc_file']['Type']  = 'doc';
          $attr['doc_file']['Label'] = 'file';
          $attr['doc_file']['Attr']  = array('size' => 35);
          //$attr['photo_file']['Caption'] = 'file'; */

        $attr['photo_name']['Type']  = 'text';
        $attr['photo_name']['Label'] = '�������� ����������';
        $attr['photo_name']['Attr']  = array('size'                         => 35);
        $attr['photo_name']['Caption'] = '�������� ����������';

        $attr['photo_comment']['Type']  = 'textarea';
        $attr['photo_comment']['Label'] = '�������� ����������';
        $attr['photo_comment']['Attr']  = array('rows' => 6, 'cols' => 39);
        //$attr['photo_comment']['Caption'] = '�������� ����������';

        $attr['photo_date']['Type']  = 'date';
        $attr['photo_date']['Label'] = '���� ��������';
        $attr['photo_date']['Attr']  = array('size'                         => 32);
        $attr['photo_date']['Caption'] = '���� ��������';

        $attr['photo_author']['Type']  = 'text';
        $attr['photo_author']['Label'] = '����� ����������';
        $attr['photo_author']['Attr']  = array('size'                           => 35);
        $attr['photo_author']['Caption'] = '�����';

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
        $this->setCaption("���� �����", "images/catalog.png");
    }

    function &getFormFields() {
        $attr = array();



        $attr['cat_name']['Type']  = 'text';
        $attr['cat_name']['Label'] = '������������';
        $attr['cat_name']['Attr']  = array('size'                       => 35);
        $attr['cat_name']['Caption'] = '������������';

        $attr['cat_mail']['Type']  = 'text';
        $attr['cat_mail']['Label'] = 'E-Mail';
        $attr['cat_mail']['Attr']  = array('size'                       => 35);
        $attr['cat_mail']['Caption'] = 'E-Mail';

        $attr['cat_cont']['Type']  = 'text';
        $attr['cat_cont']['Label'] = '���������� ����������';
        $attr['cat_cont']['Attr']  = array('size'                       => 35);
        $attr['cat_cont']['Caption'] = '���������� ����������';


        $attr['cat_resume']['Type']  = 'textarea';
        $attr['cat_resume']['Label'] = '������� ��������';
        $attr['cat_resume']['Attr']  = array('rows' => 6, 'cols' => 39);




        $attr['cat_descr']['Type']  = 'editor';
        $attr['cat_descr']['Label'] = '������ �������� ';
        $attr['cat_descr']['Attr']  = array('width'  => '100%', 'height' => '400px');


        $attr['cat_doc1']['Type']    = 'text';
        $attr['cat_dname1']['Label'] = '�������� �����-�������� �1';
        $attr['cat_dname1']['Attr']  = array('size'                     => 35);
        $attr['cat_doc1']['Type']  = 'doc';
        $attr['cat_doc1']['Label'] = '����-�������� �1';
        $attr['cat_doc1']['Attr']  = array('size' => 35);



        $attr['cat_doc2']['Type']    = 'text';
        $attr['cat_dname2']['Label'] = '�������� �����-�������� �2';
        $attr['cat_dname2']['Attr']  = array('size'                     => 35);
        $attr['cat_doc2']['Type']  = 'doc';
        $attr['cat_doc2']['Label'] = '����-�������� �2';
        $attr['cat_doc2']['Attr']  = array('size' => 35);


        $attr['cat_doc3']['Type']    = 'text';
        $attr['cat_dname3']['Label'] = '�������� �����-�������� �3';
        $attr['cat_dname3']['Attr']  = array('size'                     => 35);
        $attr['cat_doc3']['Type']  = 'doc';
        $attr['cat_doc3']['Label'] = '����-�������� �3';
        $attr['cat_doc3']['Attr']  = array('size' => 35);

        $attr['cat_doc4']['Type']    = 'text';
        $attr['cat_dname4']['Label'] = '�������� �����-�������� �4';
        $attr['cat_dname4']['Attr']  = array('size'                     => 35);
        $attr['cat_doc4']['Type']  = 'doc';
        $attr['cat_doc4']['Label'] = '����-�������� �4';
        $attr['cat_doc4']['Attr']  = array('size' => 35);


        $attr['cat_doc5']['Type']    = 'text';
        $attr['cat_dname5']['Label'] = '�������� �����-�������� �5';
        $attr['cat_dname5']['Attr']  = array('size'                     => 35);
        $attr['cat_doc5']['Type']  = 'doc';
        $attr['cat_doc5']['Label'] = '����-�������� �5';
        $attr['cat_doc5']['Attr']  = array('size' => 35);

        $attr['cat_doc6']['Type']    = 'text';
        $attr['cat_dname6']['Label'] = '�������� �����-�������� �6';
        $attr['cat_dname6']['Attr']  = array('size'                     => 35);
        $attr['cat_doc6']['Type']  = 'doc';
        $attr['cat_doc6']['Label'] = '����-�������� �6';
        $attr['cat_doc6']['Attr']  = array('size' => 35);

        $attr['cat_doc7']['Type']    = 'text';
        $attr['cat_dname7']['Label'] = '�������� �����-�������� �7';
        $attr['cat_dname7']['Attr']  = array('size'                     => 35);
        $attr['cat_doc7']['Type']  = 'doc';
        $attr['cat_doc7']['Label'] = '����-�������� �7';
        $attr['cat_doc7']['Attr']  = array('size' => 35);




        $attr['cat_doc8']['Type']    = 'text';
        $attr['cat_dname8']['Label'] = '�������� �����-�������� �8';
        $attr['cat_dname8']['Attr']  = array('size'                     => 35);
        $attr['cat_doc8']['Type']  = 'doc';
        $attr['cat_doc8']['Label'] = '����-�������� �8';
        $attr['cat_doc8']['Attr']  = array('size' => 35);

        $attr['cat_doc9']['Type']    = 'text';
        $attr['cat_dname9']['Label'] = '�������� �����-�������� �9';
        $attr['cat_dname9']['Attr']  = array('size'                     => 35);
        $attr['cat_doc9']['Type']  = 'doc';
        $attr['cat_doc9']['Label'] = '����-�������� �9';
        $attr['cat_doc9']['Attr']  = array('size' => 35);

        $attr['cat_doc10']['Type']    = 'text';
        $attr['cat_dname10']['Label'] = '�������� �����-�������� �10';
        $attr['cat_dname10']['Attr']  = array('size'                      => 35);
        $attr['cat_doc10']['Type']  = 'doc';
        $attr['cat_doc10']['Label'] = '����-�������� �10';
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
        $this->setCaption("������������� ������", "images/catalog.png");
    }

    function &getFormFields() {
        $attr = array();

        $attr['pattern_id']['Type']  = 'select';
        $attr['pattern_id']['Label'] = '��������� ������';
        $attr['pattern_id']['Attr']  = array('style'                        => 'width:230px');
        $attr['pattern_id']['DB']      = 'ref_municipal_structures';
        $attr['pattern_id']['Caption'] = '��������� ������';

        $attr['mo_number']['Type']  = 'text';
        $attr['mo_number']['Label'] = '����� ������';
        $attr['mo_number']['Attr']  = array('size'                        => 35);
        $attr['mo_number']['Caption'] = '����� ������';

        $attr['mo_curent_order']['Type']  = 'text';
        $attr['mo_curent_order']['Label'] = '������������ ������';
        $attr['mo_curent_order']['Attr']  = array('size'                              => 35);
        $attr['mo_curent_order']['Caption'] = '������������ ������';

        $attr['mo_curent_orders']['Type']  = 'text';
        $attr['mo_curent_orders']['Label'] = '��������';
        $attr['mo_curent_orders']['Attr']  = array('size'                               => 35);
        $attr['mo_curent_orders']['Caption'] = '��������';

        $attr['mo_curent_prices']['Type']  = 'text';
        $attr['mo_curent_prices']['Label'] = '����';
        $attr['mo_curent_prices']['Attr']  = array('size' => 35);
        //$attr['mo_curent_prices']['Caption'] = '����';

        $attr['mo_start_date']['Type']  = 'date';
        $attr['mo_start_date']['Label'] = '���� ������ ������ ������';
        $attr['mo_start_date']['Attr']  = array('size'                            => 32);
        $attr['mo_start_date']['Caption'] = '���� ������ ������ ������';

        $attr['mo_start_time']['Type']  = 'text';
        $attr['mo_start_time']['Label'] = '����� ������ ������ ������';
        $attr['mo_start_time']['Attr']  = array('size'                            => 35);
        $attr['mo_start_time']['Caption'] = '����� ������ ������ ������';

        $attr['mo_start_merch_date']['Type']  = 'date';
        $attr['mo_start_merch_date']['Label'] = '���� ������ ������';
        $attr['mo_start_merch_date']['Attr']  = array('size'                                  => 32);
        $attr['mo_start_merch_date']['Caption'] = '���� ������ ������';

        $attr['mo_start_merch_time']['Type']  = 'text';
        $attr['mo_start_merch_time']['Label'] = '����� ������ ������';
        $attr['mo_start_merch_time']['Attr']  = array('size'                                  => 35);
        $attr['mo_start_merch_time']['Caption'] = '����� ������ ������';

        $attr['mo_contacts']['Type']  = 'text';
        $attr['mo_contacts']['Label'] = '���������� ������ ';
        $attr['mo_contacts']['Attr']  = array('size' => 35);
        //$attr['mo_contacts']['Caption'] = '���������� ������ ';

        $attr['mo_file0_name']['Type']  = 'text';
        $attr['mo_file0_name']['Label'] = '�������� �����-�������� �1';
        $attr['mo_file0_name']['Attr']  = array('size'                     => 35);
        $attr['mo_file0']['Type']  = 'doc';
        $attr['mo_file0']['Label'] = '����-�������� �1';
        $attr['mo_file0']['Attr']  = array('size' => 35);

        $attr['mo_file1_name']['Type']  = 'text';
        $attr['mo_file1_name']['Label'] = '�������� �����-�������� �2';
        $attr['mo_file1_name']['Attr']  = array('size'                     => 35);
        $attr['mo_file1']['Type']  = 'doc';
        $attr['mo_file1']['Label'] = '����-�������� �2';
        $attr['mo_file1']['Attr']  = array('size' => 35);

        $attr['mo_file2_name']['Type']  = 'text';
        $attr['mo_file2_name']['Label'] = '�������� �����-�������� �3';
        $attr['mo_file2_name']['Attr']  = array('size'                     => 35);
        $attr['mo_file2']['Type']  = 'doc';
        $attr['mo_file2']['Label'] = '����-�������� �3';
        $attr['mo_file2']['Attr']  = array('size' => 35);

        $attr['mo_file3_name']['Type']  = 'text';
        $attr['mo_file3_name']['Label'] = '�������� �����-�������� �4';
        $attr['mo_file3_name']['Attr']  = array('size'                     => 35);
        $attr['mo_file3']['Type']  = 'doc';
        $attr['mo_file3']['Label'] = '����-�������� �4';
        $attr['mo_file3']['Attr']  = array('size' => 35);

        $attr['mo_file4_name']['Type']  = 'text';
        $attr['mo_file4_name']['Label'] = '�������� �����-�������� �5';
        $attr['mo_file4_name']['Attr']  = array('size'                     => 35);
        $attr['mo_file4']['Type']  = 'doc';
        $attr['mo_file4']['Label'] = '����-�������� �5';
        $attr['mo_file4']['Attr']  = array('size' => 35);

        $attr['mo_file5_name']['Type']  = 'text';
        $attr['mo_file5_name']['Label'] = '�������� �����-�������� �6';
        $attr['mo_file5_name']['Attr']  = array('size'                     => 35);
        $attr['mo_file5']['Type']  = 'doc';
        $attr['mo_file5']['Label'] = '����-�������� �6';
        $attr['mo_file5']['Attr']  = array('size' => 35);

        $attr['mo_file6_name']['Type']  = 'text';
        $attr['mo_file6_name']['Label'] = '�������� �����-�������� �7';
        $attr['mo_file6_name']['Attr']  = array('size'                     => 35);
        $attr['mo_file6']['Type']  = 'doc';
        $attr['mo_file6']['Label'] = '����-�������� �7';
        $attr['mo_file6']['Attr']  = array('size' => 35);

        $attr['mo_file7_name']['Type']  = 'text';
        $attr['mo_file7_name']['Label'] = '�������� �����-�������� �8';
        $attr['mo_file7_name']['Attr']  = array('size'                     => 35);
        $attr['mo_file7']['Type']  = 'doc';
        $attr['mo_file7']['Label'] = '����-�������� �8';
        $attr['mo_file7']['Attr']  = array('size' => 35);

        $attr['mo_file8_name']['Type']  = 'text';
        $attr['mo_file8_name']['Label'] = '�������� �����-�������� �9';
        $attr['mo_file8_name']['Attr']  = array('size'                     => 35);
        $attr['mo_file8']['Type']  = 'doc';
        $attr['mo_file8']['Label'] = '����-�������� �9';
        $attr['mo_file8']['Attr']  = array('size' => 35);

        $attr['mo_file9_name']['Type']  = 'text';
        $attr['mo_file9_name']['Label'] = '�������� �����-�������� �10';
        $attr['mo_file9_name']['Attr']  = array('size'                     => 35);
        $attr['mo_file9']['Type']  = 'doc';
        $attr['mo_file9']['Label'] = '����-�������� �10';
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
        $attr['user_name']['Label'] = '���';
        $attr['user_name']['Attr']  = array('size'                        => 35);
        $attr['user_name']['Caption'] = '���';

        $attr['user_login']['Type']  = 'text';
        $attr['user_login']['Label'] = '�����';
        $attr['user_login']['Attr']  = array('size'                         => 35);
        $attr['user_login']['Caption'] = '�����';

        $attr['user_type_id']['Type']    = 'select';
        $attr['user_type_id']['Label']   = '��� ������������';
        $attr['user_type_id']['DB']      = 'ref_user_type';
        $attr['user_type_id']['Caption'] = '���';
        $attr['user_type_id']['Attr']    = array('style'    => 'width:230px', 'onchange' => 'changeUserType(this)');


        $attr['user_password']['Type']  = 'password';
        $attr['user_password']['Label'] = '������';
        $attr['user_password']['Attr']  = array('size' => 35);


        $attr['user_password_confirm']['Type']  = 'password';
        $attr['user_password_confirm']['Label'] = '����������� ������';
        $attr['user_password_confirm']['Attr']  = array('size' => 35);


        $attr['user_email']['Type']  = 'text';
        $attr['user_email']['Label'] = 'E-mail';
        $attr['user_email']['Attr']  = array('size'                         => 35);
        $attr['user_email']['Caption'] = 'E-mail';

        $attr['user_date']['Type']  = 'date';
        $attr['user_date']['Label'] = '���� ��������';
        $attr['user_date']['Attr']  = array('size'                        => 32);
        $attr['user_date']['Caption'] = '���� ��������';

        $attr['user_ip']['Type']  = 'text';
        $attr['user_ip']['Label'] = 'IP';
        $attr['user_ip']['Attr']  = array('size'                      => 35);
        $attr['user_ip']['Caption'] = 'IP';


        $attr['user_permission']['Type']  = 'new';
        $attr['user_permission']['Label'] = '������ � ��������';
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
            $_SESSION['message'] = '������������ � ������ ������� ��� ����������...';
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
            $_SESSION['message'] = '������������ � ������ ������� ��� ����������...';
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
		   alert('���� \"�����\" ��������� �������...');
		   form.elements['form[user_login]'].focus();
		   return false;
		}
		if ((form.elements['form[user_password]'].value.trim() == '')||(form.elements['form[user_password]'].value != form.elements['form[user_password_confirm]'].value))
		{
		   form.elements['form[user_password]'].focus();
		   alert('��� ������������ ����� ����� \"������\" � \"����������� ������\"');
		   return false;
		}
		if (form.elements['form[user_email]'].value.trim() == '' || !checkEmail(form.elements['form[user_email]']))
		{
		   form.elements['form[user_email]'].focus();
		   alert('������ ����� ������ ����������� �����');
		   return false;
		}
		var permits = getPermits()
		form.elements['form[user_permit_hidden]'].value = permits;
		if (permits.indexOf('@') < 3 && form.elements['form[user_type_id]'].value != 1) {
		 alert('�������� ��� ���� ������ ��� ������� � �������� �����');
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