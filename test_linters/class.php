<?php
// Создаем новый класс Coor:
class Coor {
// данные (свойства):
var $name;

// методы:
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

// Создаем объект класса Coor:
$object = new Coor;
// Теперь для изменения имени используем метод Setname():
$object->Setname("Nick");
// А для доступа, как и прежде, Getname():
$object->Getname();
// Сценарий выводит 'Nick'
?>