/*
Items-System by Games, 2013.
Programming language: Pawn


Добавление новых предметов:

В паблик OnFilterScriptInit() добавлять через ф-ию AddItem("название предмета")

Например, паблик может иметь такой вид:

public OnFilterScriptInit()
{
	AddItem("Яблоко");
	AddItem("Груша");
	AddItem("Помидор");
	AddItem("Мандарин");
}

При включенном режиме DEBUG в консоль при добавлении объекта указывается их ид.

Изменять кол-во предметов игроку можно через 2 функции:
stock SetItemTheName(playerid, string[], Val); //Изменить кол-во предметов игроку по названию предмета
stock SetItemTheID(playerid, oID, Val); //Изменить кол-во предметов игроку по ид предмета

Действия при использовании прописывать здесь:
stock UseItem(playerid, itID); //использование предметов
*/

#include <a_samp>
#include <mxINI>

#define MAX_ITEMS 100
#define DEBUG 1
#define D_Items 500 //Первый ид диалога системы

//Прототипы функций:
stock AddItem(string[]); //добавить предмет в список предметов
stock SetItemTheName(playerid, string[], Val); //Изменить кол-во предметов игроку по названию предмета
stock SetItemTheID(playerid, oID, Val); //Изменить кол-во предметов игроку по ид предмета
stock ResetItems(playerid); //обнуляет предметы игроку
stock LoadItems(playerid); //загрузка предметов
stock UseItem(playerid, itID); //использование предметов

//--------------
//Внутренние ф-ии:
stock ShiftData(playerid, ItS); //сдвинуть данные у игрока при пустом слоте

enum I_Info
{
	Name[30]
}

new Items[MAX_ITEMS][I_Info];
new PlayerItems[MAX_PLAYERS][MAX_ITEMS][2]; // 0 - ID | 1 - кол-во
new UseSlotItem[MAX_PLAYERS]; //слот при выборках
new GiveInfo[MAX_PLAYERS][4];

new ItemID = 0x0;

public OnFilterScriptInit()
{
	AddItem("Яблоко");
	AddItem("Груша");
	AddItem("Помидор");
	AddItem("Мандарин");
}

public OnPlayerConnect(playerid)
{
	ResetItems(playerid);
	LoadItems(playerid);
}

public OnPlayerDisconnect(playerid, reason)
{
    SaveItems(playerid);
}

public OnPlayerCommandText(playerid, cmdtext[])
{
	if(!strcmp("/inv", cmdtext, true))
	{
		new string[1000], find = 0; //изменить при необходимости
		for(new f; f != ItemID; f++)
		{
  			if(PlayerItems[playerid][f][0] != 0)
	 		{
	 		    find = 1;
	 		    format(string, sizeof(string), "%s%s [%d]\n", string, Items[PlayerItems[playerid][f][0] - 1][Name], PlayerItems[playerid][f][1]);
	 		}
	 		else break;
		}
		if(find == 1) ShowPlayerDialog(playerid, D_Items, DIALOG_STYLE_LIST, "Инвертарь", string, "ОК", "Отмена");
		else SendClientMessage(playerid, -1, "У Вас нет предметов.");
		return 1;
	}
	if(!strcmp("/giveinv", cmdtext, true))
	{
	    SetItemTheID(playerid, 1, 20);
	    SetItemTheID(playerid, 2, 20);
	    SetItemTheID(playerid, 3, 20);
	    SetItemTheID(playerid, 4, 20);
	    return true;
	}
	return 0;
}

public OnDialogResponse(playerid, dialogid, response, listitem, inputtext[])
{
	if(dialogid == D_Items && response)
	{
        UseSlotItem[playerid] = listitem;
        ShowPlayerDialog(playerid, D_Items + 1, DIALOG_STYLE_LIST, "Инвертарь", "Использовать\nВыкинуть\nПередать", "ОК", "Отмена");
	}
	else if(dialogid == D_Items + 1 && response)
	{
	    switch(listitem)
	    {
	        case 0: //использовать
	        {
	            //код использования
	            UseItem(playerid, PlayerItems[playerid][UseSlotItem[playerid]][0]);
	            SetItemTheID(playerid, PlayerItems[playerid][UseSlotItem[playerid]][0], -1);
	            SendClientMessage(playerid, -1, "Вы успешно использовали предмет");
	        }
			case 1: //выкинуть предмет
			{
				ShowPlayerDialog(playerid, D_Items + 3, DIALOG_STYLE_INPUT, "Выкинуть предмет", "Введите кол-во предметов, которые Вы хотите выкинуть:", "Ок", "Отмена");
			}
			case 2: //передать предмет
			{
			    ShowPlayerDialog(playerid, D_Items + 2, DIALOG_STYLE_INPUT, "Передать предмет", "Введите ид игрока:", "Ок", "Отмена");
			}
	    }
	}
	else if(dialogid == D_Items + 2 && response)
	{
	    new id = strval(inputtext);
		if(!IsPlayerConnected(id)) return SendClientMessage(playerid, -1, "Данный игрок не подключён.");
		new Float: Pos[3];
		GetPlayerPos(id, Pos[0], Pos[1], Pos[2]);
		if(!IsPlayerInRangeOfPoint(playerid, 5.00, Pos[0], Pos[1], Pos[2])) return SendClientMessage(playerid, -1, "Вы должны находиться возле игрока.");
		GiveInfo[playerid][0] = id;
	    ShowPlayerDialog(playerid, D_Items + 4, DIALOG_STYLE_INPUT, "Передать предмет", "Введите кол-во предметов:", "Ок", "Отмена");
	}
	else if(dialogid == D_Items + 3 && response)
	{
	    if(!strlen(inputtext)) return SendClientMessage(playerid, -1, "Вы ничего не ввели");
	    if(strval(inputtext) > PlayerItems[playerid][UseSlotItem[playerid]][1]) return SendClientMessage(playerid, -1, "У Вас нет столько предметов.");
	    SetItemTheID(playerid, PlayerItems[playerid][UseSlotItem[playerid]][0], -strval(inputtext));
		SendClientMessage(playerid, -1, "Вы успешно выкинули предмет(ы)");
	}
	else if(dialogid == D_Items + 4 && response)
	{
		if(!strlen(inputtext)) return SendClientMessage(playerid, -1, "Вы ничего не ввели");
	    if(strval(inputtext) > PlayerItems[playerid][UseSlotItem[playerid]][1]) return SendClientMessage(playerid, -1, "У Вас нет столько предметов.");
		GiveInfo[playerid][1] = strval(inputtext);
		ShowPlayerDialog(playerid, D_Items + 5, DIALOG_STYLE_INPUT, "Передать предмет", "Введите cумму:", "Ок", "Отмена");
	}
	else if(dialogid == D_Items + 5 && response)
	{
		if(!strlen(inputtext)) return SendClientMessage(playerid, -1, "Вы ничего не ввели");
		if(strval(inputtext) < 1) return SendClientMessage(playerid, -1, "Нельзя вводить отрицательную сумму.");
		if(!IsPlayerConnected(GiveInfo[playerid][0])) return SendClientMessage(playerid, -1, "Данный игрок не подключён.");


		new SendStr[128], Names[MAX_PLAYER_NAME];
		GetPlayerName(playerid, Names, MAX_PLAYER_NAME);
		//printf("Продает: %d | Покупает: %d", GiveInfo[playerid][0], playerid);
		format(SendStr, sizeof(SendStr), "%s [%d] предлагает купить %s [%d] за $%d.\nВы хотите купить это?", Names, playerid, Items[PlayerItems[playerid][UseSlotItem[playerid]][0]][Name], GiveInfo[playerid][1], strval(inputtext));
		ShowPlayerDialog(GiveInfo[playerid][0], D_Items + 6, DIALOG_STYLE_MSGBOX, "Покупка предмета", SendStr, "Да", "Нет");

		GiveInfo[GiveInfo[playerid][0]][0] = playerid; //ид игрока
		GiveInfo[GiveInfo[playerid][0]][1] = GiveInfo[playerid][1]; //кол-во
		GiveInfo[GiveInfo[playerid][0]][2] = strval(inputtext);//цена
		GiveInfo[GiveInfo[playerid][0]][3] = UseSlotItem[playerid]; //слот у игрока
	}
	else if(dialogid == D_Items + 6 && response)
	{
		if(!IsPlayerConnected(GiveInfo[playerid][0])) return SendClientMessage(playerid, -1, "Данный игрок не подключён.");
		//printf("Продает: %d | Покупает: %d", GiveInfo[playerid][0], playerid);
        SetItemTheID(playerid, PlayerItems[GiveInfo[playerid][0]][GiveInfo[playerid][3]][0], GiveInfo[playerid][1]);
		SetItemTheID(GiveInfo[playerid][0], PlayerItems[GiveInfo[playerid][0]][GiveInfo[playerid][3]][0], -GiveInfo[playerid][1]);


		new SendStr[128], Names[2][MAX_PLAYER_NAME];
		GetPlayerName(playerid, Names[0], MAX_PLAYER_NAME); //который купил
		GetPlayerName(GiveInfo[playerid][0], Names[1], MAX_PLAYER_NAME); //который продал

		format(SendStr, sizeof(SendStr), "Вы продали предмет(ы) игроку %s [%d] за $%d.", Names[0], playerid, GiveInfo[playerid][2]);
		SendClientMessage(GiveInfo[playerid][0], -1, SendStr);
		format(SendStr, sizeof(SendStr), "Вам передал предмет(ы) игрок %s [%d] за $%d.", Names[1], GiveInfo[playerid][0], GiveInfo[playerid][2]);
		SendClientMessage(playerid, -1, SendStr);
	}
	return 0;
}

stock AddItem(string[]) //добавить предмет в список предметов
{
	if(ItemID >= MAX_ITEMS) return printf("ERROR: Превышен лимит предметов.");
	#if DEBUG
	printf("Предмет: %s | ID: %d", string, ItemID + 1);
	#endif
	format(Items[ItemID][Name], 30, "%s", string);
	ItemID++;
	return 1;
}

stock SetItemTheName(playerid, string[], Val) //Изменить кол-во предметов игроку по названию предмета
{
	for(new i; i != ItemID; i++)
	{
		if(!strcmp(Items[i][Name], string, true))
		{
   			for(new f; f != ItemID; f++)
			{
			    if(PlayerItems[playerid][f][0] == i + 1 || PlayerItems[playerid][f][0] == 0)
			    {
			        PlayerItems[playerid][f][0] = i + 1;
			        PlayerItems[playerid][f][1] += Val;
					ShiftData(playerid, f);
			        return 1;
			    }
			}
		}
	}
	printf("Предмет %s - не существует!", string);
	return 1;
}

stock SetItemTheID(playerid, oID, Val) //Изменить кол-во предметов игроку по ид предмета
{
    if(oID < 1 || oID > ItemID) return printf("ERROR: Данный предмет не существует.");

	
	for(new f; f != ItemID; f++)
	{
 		if(PlayerItems[playerid][f][0] == oID || PlayerItems[playerid][f][0] == 0)
 		{
 			PlayerItems[playerid][f][0] = oID;
 			PlayerItems[playerid][f][1] += Val;
 			ShiftData(playerid, f);
 			break;
 		}
	}
	return 1;
}

stock ShiftData(playerid, ItS) //сдвинуть данные у игрока при пустом слоте
{
	if(PlayerItems[playerid][ItS][1] != 0) return 1; //предметы есть, сдвиг не нужен

	PlayerItems[playerid][ItS][0] = 0;

	if(ItS == ItemID) return 1; //последний элемент, сдвиг не нужен
	for(new i = ItS; i != ItemID; i++)
	{
		PlayerItems[playerid][i][0] = PlayerItems[playerid][i + 1][0];
 		PlayerItems[playerid][i][1] = PlayerItems[playerid][i + 1][1];
	}
	return 1;
}

stock ResetItems(playerid) //обнуляет предметы игроку
{
	for(new i = 0; i != ItemID; i++)
	{
		PlayerItems[playerid][i][0] = 0;
 		PlayerItems[playerid][i][1] = 0;
	}
}

stock LoadItems(playerid) //загрузка предметов
{
	new PLName[MAX_PLAYER_NAME], FileAddr[MAX_PLAYER_NAME + 20], temp[2][30];
	GetPlayerName(playerid, PLName, MAX_PLAYER_NAME);
	format(FileAddr, sizeof(FileAddr), "Items/%s.ini", PLName);
	if(fexist(FileAddr))
 	{
		new file = ini_openFile(FileAddr);
		new Slot, val, UseSlot;
		for(new f; f != ItemID; f++)
		{
            format(temp[0], 30,"Item%d", f);
            ini_getString(file, temp[0], temp[1]);
            for(new j, l = strlen(temp[1]), pos, temps[5]; j != l; j++)
            {
                if(temp[1][j] == '|')
                {
                    pos = 0;
                    Slot = strval(temps);
                    for(new m; m != sizeof(temps); m++) temps[m] = '\0';
                    continue;
                }
                temps[pos] = temp[1][j];
                pos++;
                if(j == l - 1) val = strval(temps);
            }

            if(val != 0)
   			{
   			    printf("%i %i | %i", f, Slot, val);
				PlayerItems[playerid][UseSlot][0] = Slot;
    			PlayerItems[playerid][UseSlot][1] = val;
	      		UseSlot++;
   			}
   			val = 0;
		}
		ini_closeFile(file);
	}
	else
	{
		new file = ini_createFile(FileAddr);
		ini_setString(file, "Item0", "0|0");
		ini_closeFile(file);
	}
}

stock SaveItems(playerid) //сохранение предметов
{
	new PLName[MAX_PLAYER_NAME], FileAddr[MAX_PLAYER_NAME + 20], temp[2][30];
	GetPlayerName(playerid, PLName, MAX_PLAYER_NAME);
	format(FileAddr, sizeof(FileAddr), "Items/%s.ini", PLName);
	new file = ini_openFile(FileAddr);
	for(new f; f != ItemID; f++)
	{
		format(temp[0], 30,"Item%d", f);
		format(temp[1], 30,"%d|%d", PlayerItems[playerid][f][0], PlayerItems[playerid][f][1]);
		ini_setString(file, temp[0], temp[1]);
	}
	ini_closeFile(file);
}

stock UseItem(playerid, itID) //использование предметов
{
	switch(itID)
	{
	    case 1: //действие для предмета 1
	    {
	        new Float:PH;
		    GetPlayerHealth(playerid, PH);
		    SetPlayerHealth(playerid, PH +10);
	    }
	    case 2: //действие для предмета 2
	    {

	    }
	    //и так далее
	}
}
