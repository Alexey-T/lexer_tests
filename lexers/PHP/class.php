<?php
// ������� ����� ����� Coor:
class Coor {
// ������ (��������):
var $name;

// ������:
 function Getname(){
   [ 
    ]
 echo $this->name;
 }

 function Setname($name) {
 $this->name = $name;
 $this->addCssFile($css); 
 $var=aaa;
 }

}

// ������� ������ ������ Coor:
$object = new Coor;
// ������ ��� ��������� ����� ���������� ����� Setname():
$object->Setname("Nick");
// � ��� �������, ��� � ������, Getname():
$object->Getname();
// �������� ������� 'Nick'
?>