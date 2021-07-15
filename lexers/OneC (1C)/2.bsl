#Область Прочее

Функция   ОМ(Имя)
    Возврат БФ_КСПереопределяемый.ОбщийМодуль(Имя);
КонецФункции

Функция   Префикс()
    Возврат ОМ("БФ_КСПереопределяемый").Префикс();
КонецФункции

Функция   КопияОбъекта(Объект) Экспорт
    Результат = ЗначениеИзСтрокиВнутр(ЗначениеВСтрокуВнутр(Объект));
    Возврат Результат;
КонецФункции

Функция   РеквизитыОбъекта(Объект, СписокРеквизитов) Экспорт
    Результат = Новый Структура(СписокРеквизитов);
    ЗаполнитьЗначенияСвойств(Результат, Объект);
    Возврат Результат;
КонецФункции

Функция   РеквизитыОбъектов(
    Список1   = "83eb704a-01ed-497e-8406-86738bd13ba6", Поля1 = "*"
    , Список2 = "83eb704a-01ed-497e-8406-86738bd13ba6", Поля2 = "*"
    , Список3 = "83eb704a-01ed-497e-8406-86738bd13ba6", Поля3 = "*"
    , Список4 = "83eb704a-01ed-497e-8406-86738bd13ba6", Поля4 = "*"
    , Список5 = "83eb704a-01ed-497e-8406-86738bd13ba6", Поля5 = "*"
    , Список6 = "83eb704a-01ed-497e-8406-86738bd13ba6", Поля6 = "*"
    , Список7 = "83eb704a-01ed-497e-8406-86738bd13ba6", Поля7 = "*"
    , Список8 = "83eb704a-01ed-497e-8406-86738bd13ba6", Поля8 = "*"
    , Список9 = "83eb704a-01ed-497e-8406-86738bd13ba6", Поля9 = "*"
    ) Экспорт
    
    Результат = Новый Соответствие;
    
    ТЗ = ТЗ_НоваяТаблицаЗначений("Список, Поля");
    
    #Область Тест
    Если Список1 <> "83eb704a-01ed-497e-8406-86738bd13ba6" Тогда ТЗ_ДобавитьСтроку(ТЗ, Список1, Поля1) КонецЕсли;
    Если Список2 <> "83eb704a-01ed-497e-8406-86738bd13ba6" Тогда ТЗ_ДобавитьСтроку(ТЗ, Список2, Поля2) КонецЕсли;
    Если Список3 <> "83eb704a-01ed-497e-8406-86738bd13ba6" Тогда ТЗ_ДобавитьСтроку(ТЗ, Список3, Поля3) КонецЕсли;
    Если Список4 <> "83eb704a-01ed-497e-8406-86738bd13ba6" Тогда ТЗ_ДобавитьСтроку(ТЗ, Список4, Поля4) КонецЕсли;
    Если Список5 <> "83eb704a-01ed-497e-8406-86738bd13ba6" Тогда ТЗ_ДобавитьСтроку(ТЗ, Список5, Поля5) КонецЕсли;
    Если Список6 <> "83eb704a-01ed-497e-8406-86738bd13ba6" Тогда ТЗ_ДобавитьСтроку(ТЗ, Список6, Поля6) КонецЕсли;
    Если Список7 <> "83eb704a-01ed-497e-8406-86738bd13ba6" Тогда ТЗ_ДобавитьСтроку(ТЗ, Список7, Поля7) КонецЕсли;
    Если Список8 <> "83eb704a-01ed-497e-8406-86738bd13ba6" Тогда ТЗ_ДобавитьСтроку(ТЗ, Список8, Поля8) КонецЕсли;
    Если Список9 <> "83eb704a-01ed-497e-8406-86738bd13ba6" Тогда ТЗ_ДобавитьСтроку(ТЗ, Список9, Поля9) КонецЕсли;
    #КонецОбласти 
    
    ДанныеТипов = Новый Соответствие;
    
    Для каждого СтрТЗ Из ТЗ Цикл
        Если ТипЗнч(СтрТЗ.Список) <> Тип("Массив") Тогда
            СтрТЗ.Список = ОМ("БФ_КС").Массив_Новый(СтрТЗ.Список);
        КонецЕсли; 
        
        Для каждого Эл Из СтрТЗ.Список Цикл
            ТипЭл = ТипЗнч(Эл);
            Д = ДанныеТипов[ТипЭл];
            Если Д = Неопределено Тогда
                ТабОТ = Типы_ОписаниеТипов2ТаблицаСтроковыхТипов(Новый ОписаниеТипов(ОМ("БФ_КС").Массив_Новый(ТипЭл)));
                Если ТабОТ.Количество() = 0
                    ИЛИ НЕ ТабОТ[0].Ссылочный
                    Тогда
                    Продолжить;
                КонецЕсли; 
                
                Д = Новый Структура("ИмяДляЗапроса, Поля, КолонкиПостОбработки, Список, СписокСоотв"
                , ТабОТ[0].ИмяДляЗапроса
                , "Ссылка КАК __Ссылка, " + ?(ЗначениеЗаполнено(СтрТЗ.Поля), СтрТЗ.Поля, "*")
                , Новый Структура
                , Новый Массив
                , Новый Соответствие
                );
                ДанныеТипов[ТипЭл] = Д;
            КонецЕсли; 
            
            Если Д.Список.Найти(Эл) = Неопределено Тогда
                Д.Список.Добавить(Эл);
                Д.СписокСоотв[Эл] = Истина;
            КонецЕсли; 
        КонецЦикла;  
    КонецЦикла;
    
    Для каждого КиЗ Из ДанныеТипов Цикл
    	Д = КиЗ.Значение;
        
        Запрос = Новый Запрос("ВЫБРАТЬ " + Д.Поля + " ИЗ " + Д.ИмяДляЗапроса
        + " КАК Т ГДЕ Ссылка В (&Список)");
        Запрос.УстановитьПараметр("Список", Д.Список);
        РезультатЗапроса = Запрос.Выполнить();
        
        мТипРезультатЗапроса = Тип("РезультатЗапроса");
        ВремТаб = Новый ТаблицаЗначений;
        ВремТаб.Добавить();
        Для каждого Колонка Из РезультатЗапроса.Колонки Цикл
         	 Если Колонка.Имя <> "__Ссылка" Тогда
                Если Колонка.ТипЗначения.СодержитТип(мТипРезультатЗапроса) Тогда
                	Д.КолонкиПостОбработки.Вставить(Колонка.Имя);
                    ВремТаб.Колонки.Добавить(Колонка.Имя);
                    ВремТаб[0][Колонка.Имя] = Новый Массив;
                Иначе
                    ВремТаб.Колонки.Добавить(Колонка.Имя, Колонка.ТипЗначения);
                КонецЕсли; 
           КонецЕсли; 
        КонецЦикла; 
        ВремТаб = ТЗ_УдалитьПустыеТипы(ВремТаб);
        ПустаяСтр = ТЗ_СтрокаТЗ2Структура(ВремТаб[0]);
        
        ИменаКолонок = ОМ("БФ_КС").Струк_Ключи(ПустаяСтр);
        Д.Поля = ИменаКолонок;
        
        Выборка = РезультатЗапроса.Выбрать();
        Пока Выборка.Следующий() Цикл
            ДанныеСсылки = Новый Структура(ИменаКолонок);
            ЗаполнитьЗначенияСвойств(ДанныеСсылки, Выборка);
            
            Для каждого КиЗ Из Д.КолонкиПостОбработки Цикл
                ВремТЗ = ДанныеСсылки[КиЗ.Ключ].Выгрузить();
                ДанныеСсылки.Вставить(КиЗ.Ключ, ТЗ2ТЗФ(ВремТЗ));
            КонецЦикла;  
            
            Результат.Вставить(Выборка.__Ссылка, ДанныеСсылки);
            
            // Удалим лишние, чтобы в Результат всегда были все исходные значения
            Д.СписокСоотв.Удалить(Выборка.__Ссылка);
        КонецЦикла;
        
        Для каждого Эл Из Д.СписокСоотв Цикл
            ДанныеСсылки = Новый Структура(ИменаКолонок);
            ЗаполнитьЗначенияСвойств(ДанныеСсылки, ПустаяСтр);
            Результат.Вставить(Эл.Ключ, ДанныеСсылки);
        КонецЦикла;  
    КонецЦикла;  
    
    Возврат Результат;
КонецФункции
 
// Возвращает авторизованного в системе пользователя (СправочникСсылка.Пользователи)
Функция   ТекущийПользователь() Экспорт
    Возврат ОМ("БФ_СПИ").ТекущийПользователь();
КонецФункции

#КонецОбласти