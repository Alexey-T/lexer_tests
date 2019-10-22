#define FILTERSCRIPT


// INCLUDES //
#include <a_samp>
#include <ZCMD>
#include <sscanf2>
#include <streamer>

// DEFINES //
#define COL_WHITE "{FFFFFF}"
#define COL_YELLOW "{F3FF02}"
#define COL_RED "{F81414}"
#define COL_BLUE "{0000FF}"

#define COLOR_BITEM 		0xE1B0B0FF
#define COLOR_GRAD1 		0xB4B5B7FF
#define COLOR_GRAD2 		0xBFC0C2FF
#define COLOR_GRAD3 		0xCBCCCEFF
#define COLOR_GRAD4 		0xD8D8D8FF
#define COLOR_GRAD5 		0xE3E3E3FF
#define COLOR_GRAD6 		0xF0F0F0FF
#define COLOR_GREY 			0xAFAFAFAA
#define COLOR_GREEN 		0x33AA33AA
#define COLOR_RED 			0xAA3333AA
#define COLOR_BLACK         0x000001FF
#define COLOR_BLUE 			0x007BD0FF
#define COLOR_LIGHTORANGE 	0xFFA100FF
#define COLOR_FLASH 		0xFF000080
#define COLOR_LIGHTRED 		0xFF6347AA
#define COLOR_LIGHTBLUE 	0x33CCFFAA
#define COLOR_LIGHTGREEN 	0x9ACD32AA
#define COLOR_YELLOW 		0xFFFF00AA
#define COLOR_LIGHTYELLOW	0xFFFF91FF
#define COLOR_YELLOW2 		0xF5DEB3AA
#define COLOR_WHITE 		0xFFFFFFAA
#define COLOR_FADE1 		0xE6E6E6E6
#define COLOR_FADE2 		0xC8C8C8C8
#define COLOR_FADE3 		0xAAAAAAAA
#define COLOR_FADE4 		0x8C8C8C8C
#define COLOR_FADE5 		0x6E6E6E6E
#define COLOR_PURPLE 		0xC2A2DAAA
#define COLOR_DBLUE 		0x2641FEAA
#define COLOR_DOC 			0xFF8282AA
#define COLOR_DCHAT 		0xF0CC00FF
#define COLOR_NEWS 			0xFFA500AA
#define COLOR_OOC 			0xE0FFFFAA
#define TEAM_BLUE_COLOR 	0x8D8DFF00
#define TEAM_GROVE_COLOR 	0x00AA00FF
#define TEAM_AZTECAS_COLOR 	0x01FCFFC8
#define NEWBIE_COLOR 		0x7DAEFFFF
#define SAMP_COLOR			0xAAC4E5FF
#define COLOR_FAMILY        0x00E6FFC8
#define COLOR_DEPARTMENT    0xFFB300C8
#define COLOR_RADIO         0x996892FF
#define COLOR_FBI           0xF097E2FF
#define COLOR_LSPD          0x041DFBC8
#define COLOR_ADMINCHAT     0xFFB300FF

// FORWARDS //
forward GiveRandomJob(playerid);
forward GiveDropOff(playerid);

// VARIABLES //
new bool:WantsJob[MAX_PLAYERS];
new bool:HasPilotJob[MAX_PLAYERS];
new bool:IsPlayerInJob[MAX_PLAYERS];
new PilotCars[8];
new Checkpoints[MAX_PLAYERS][8];
new bool:PilotJobs[MAX_PLAYERS][5];
new bool:WantsQuit[MAX_PLAYERS];
new LoadingTimer[MAX_PLAYERS];

// PICKUPS //
new jobpickup;


// STOCKS //
stock IsPilotCar(vehicleid)
{
	for(new veh = 0; veh < sizeof(PilotCars); veh++)
	{
	    if(vehicleid == PilotCars[veh]) return 1;
	}
	return 0;
}

stock GetName(playerid)
{
	new name[MAX_PLAYER_NAME];
	GetPlayerName(playerid, name, sizeof(name));
	return name;
}

// PUBLICS //
public GiveRandomJob(playerid)
{
	for(new q = 0; q < sizeof(Checkpoints[]); q++)
		Checkpoints[playerid][q] = -1;
	for(new a = 0; a < 9; a++)
		RemovePlayerMapIcon(playerid, a);
	for(new i = 0; i < sizeof(PilotJobs[]); i ++)
		PilotJobs[playerid][i] = false;
	IsPlayerInJob[playerid] = true;
	static	const	vtext[3][11] = {{"пассажиров"}, {"груз"}, {"еду"}},
					airport[4][24] = {{"аэропорта Лос-Сантоса"}, {"аэропотра Сан-Фиерро"}, {"Заброшенного аэропорта"}, {"аэропорта Лас-Вентураса"}},
					Float:points[4][3] = {{1899.1376,-2354.3645,13.5469}, {-1224.5371,11.7382,14.1484}, {365.3524,2537.1438,16.6648}, {1301.4873,1356.2103,10.8203}},
					pilot_job_str1[] = "* {FF0000}Подберите {0000FF}",
					pilot_job_str2[] = " {FF0000}до {00FF00}";
	new rand = random(4),
		string[sizeof(pilot_job_str1)-1+sizeof(vtext[])+sizeof(pilot_job_str2)-1+sizeof(airport[])];
	Checkpoints[playerid][rand] = CreateDynamicCP(points[rand][0], points[rand][1], points[rand][2], 15.0, 0, 0, 0, 100.0);
	SetPlayerMapIcon(playerid, rand, points[rand][0], points[rand][1], points[rand][2], 0, COLOR_RED, MAPICON_GLOBAL);
	PilotJobs[playerid][rand] = true;
	string = pilot_job_str1;
	strcat(string, vtext[random(3)]);
	strcat(string, pilot_job_str2);
	strcat(string, airport[rand]);
	return SendClientMessage(playerid, COLOR_LIGHTRED, string);
}

public GiveDropOff(playerid)
{
	KillTimer(LoadingTimer[playerid]);
	TogglePlayerControllable(playerid, true);
	DisablePlayerCheckpoint(playerid);
	for(new q = 0; q < sizeof(Checkpoints[]); q++)
		Checkpoints[playerid][q] = -1;
	for(new a = 0; a < 9; a++)
		RemovePlayerMapIcon(playerid, a);
	for(new i = 0; i < sizeof(PilotJobs[]); i ++)
		PilotJobs[playerid][i] = false;
	IsPlayerInJob[playerid] = false;
	new rand = random(10000), string[70];
	GivePlayerMoney(playerid, rand);
	GameTextForPlayer(playerid, "~r~YCЊEЋHA• ѓOCTABKA", 3000, 3);
	SendClientMessage(playerid, COLOR_YELLOW, "* Босс: Отличная работа, вот твои деньги.");
	format(string,sizeof(string),"* Вы успешно доставили транспорт и заработали $%d от босса.", rand);
	return SendClientMessage(playerid, COLOR_LIGHTBLUE, string);
}

#if defined FILTERSCRIPT

public OnPlayerEnterDynamicCP(playerid, checkpointid)
{
	    if(checkpointid == Checkpoints[playerid][0] || checkpointid == Checkpoints[playerid][1] || checkpointid == Checkpoints[playerid][2] || checkpointid == Checkpoints[playerid][3])
	    {
			for(new q = 0; q < sizeof(Checkpoints[]); q++)
				Checkpoints[playerid][q] = -1;
			for(new a = 0; a < 9; a++)
				RemovePlayerMapIcon(playerid, a);
	        DisablePlayerCheckpoint(playerid);
			static	const	airport[4][24] = {{"аэропорту Лос-Сантоса"}, {"аэропотру Сан-Фиерро"}, {"Заброшенном аэропорту"}, {"аэропорту Лас-Вентураса"}},
							Float:points[4][3] = {{1899.1376,-2354.3645,13.5469}, {-1224.5371,11.7382,14.1484}, {365.3524,2537.1438,16.6648}, {1301.4873,1356.2103,10.8203}},
							pilot_job_str1[] = "* {FF0000}Посадите самолёт в {00FF00}";
			new rand = random(4),
				string[sizeof(pilot_job_str1)+sizeof(airport[])];
			Checkpoints[playerid][4+rand] = CreateDynamicCP(points[rand][0], points[rand][1], points[rand][2], 15.0, 0, 0, 0, 100.0);
			SetPlayerMapIcon(playerid, 4+rand, points[rand][0], points[rand][1], points[rand][2], 0, COLOR_RED, MAPICON_GLOBAL);
			PilotJobs[playerid][rand] = true;
			string = pilot_job_str1;
			strcat(string, airport[rand]);
			return SendClientMessage(playerid, COLOR_LIGHTRED, string);
		}
		else if(checkpointid == Checkpoints[playerid][4] || checkpointid == Checkpoints[playerid][5] || checkpointid == Checkpoints[playerid][6] || checkpointid == Checkpoints[playerid][7])
		{
		    TogglePlayerControllable(playerid, false);
		    for(new i = 0; i < 9; i++) { RemovePlayerMapIcon(playerid, i); }
			LoadingTimer[playerid] = SetTimerEx("GiveDropOff", 5000, 0, "d", playerid);
			return GameTextForPlayer(playerid, "~w~PA€~r~‚PY€KA", 5000, 3);
		}
		return 1;
}

public OnFilterScriptInit()
{
	print("Pilot FS by Da Noob succesfully loaded.");
	
	// JOB PICKUP
	jobpickup = CreatePickup(1239, 1, 1958.3483,-2182.4187,13.5469);
	Create3DTextLabel("Введите /join, чтобы устроиться на работу пилотом", COLOR_GREEN, 1958.3483,-2182.4187,13.5469, 10.0, 0, 0); // 3D TEXT LABEL FOR INFO
	
	// AIRPLANES
	PilotCars[0] = AddStaticVehicleEx(519,1889.6531,-2629.1267,14.4657,0,-1,-1,900); // Shamal 1
	PilotCars[1] = AddStaticVehicleEx(519,1823.1190,-2629.1267,14.4657,0,-1,-1,900); // Shamal 2
	PilotCars[2] = AddStaticVehicleEx(519,1754.5325,-2629.1267,14.4657,0,-1,-1,900); // Shamal 3
	PilotCars[3] = AddStaticVehicleEx(487,1964.2646,-2629.1267,13.7619,0,-1,-1,900); // Heli 1
	PilotCars[4] = AddStaticVehicleEx(487,1944.7985,-2629.1267,13.7354,0,-1,-1,900); // Heli 2
	PilotCars[5] = AddStaticVehicleEx(417,1765.6274,-2285.0801,26.8743,0,-1,-1,900); // Levi 1
	PilotCars[6] = AddStaticVehicleEx(593,1616.7531,-2627.9131,14.0094,0,-1,-1,900); // Dodo 1
	PilotCars[7] = AddStaticVehicleEx(593,1681.9587,-2627.9131,14.0094,0,-1,-1,900); // Dodo 2
	
	return 1;
}

public OnPlayerConnect(playerid)
{
	WantsJob[playerid] = false;
	HasPilotJob[playerid] = false;
	IsPlayerInJob[playerid] = false;
	WantsQuit[playerid] = false;
	
	for(new i = 0; i < sizeof(PilotJobs[]); i ++)
		PilotJobs[playerid][i] = false;
	return 1;
}

public OnPlayerEnterVehicle(playerid, vehicleid, ispassenger)
{
	if(IsPilotCar(vehicleid))
	{
	    if(HasPilotJob[playerid] == false)
	    {
	        new Float:x, Float:y, Float:z;
	        GetPlayerPos(playerid, x, y, z);
	        SetPlayerPos(playerid, x, y, z);
	        SendClientMessage(playerid, COLOR_GREY, "Вы должны работать пилотом, чтобы управлять самолётом.");
		}
	}
	return 1;
}

public OnPlayerExitVehicle(playerid, vehicleid)
{
	if(HasPilotJob[playerid])
	{
	    if(PilotJobs[playerid][0] || PilotJobs[playerid][1] || PilotJobs[playerid][2] || PilotJobs[playerid][3])
	    {
			for(new q = 0; q < sizeof(Checkpoints[]); q++)
				Checkpoints[playerid][q] = -1;
			for(new a = 0; a < 9; a++)
				RemovePlayerMapIcon(playerid, a);
			for(new i = 0; i < sizeof(PilotJobs[]); i ++)
				PilotJobs[playerid][i] = false;
	        IsPlayerInJob[playerid] = false;
			SendClientMessage(playerid, COLOR_YELLOW, "* Босс: Почему не долетел до аэропорта? Из твоей зарплаты будет вычтен штраф!");
			GivePlayerMoney(playerid, -1000);
			SendClientMessage(playerid, COLOR_LIGHTBLUE, "* Ваш босс вычел 1000$ из вашей зарплаты.");
		}
	}
	return 1;
}

public OnPlayerPickUpPickup(playerid, pickupid)
{
	if(pickupid == jobpickup)
	    GameTextForPlayer(playerid, "Њ…‡OT", 3000, 3);
	return 1;
}

CMD:join(playerid)
{
	if(IsPlayerInRangeOfPoint(playerid, 5.0, 1958.3483,-2182.4187,13.5469))
	{
	    if(HasPilotJob[playerid] == false)
	    {
	    	SendClientMessage(playerid, COLOR_LIGHTBLUE, "* Вы хотите устроиться пилотом? (введите /accept job, чтобы продолжить)");
	    	WantsJob[playerid] = true;
		}
		else
		    SendClientMessage(playerid, COLOR_GRAD2, "Вы уже устроились на работу.");
	}
	return 1;
}

CMD:quitjob(playerid)
{
	if(HasPilotJob[playerid])
	{
	    SendClientMessage(playerid, COLOR_LIGHTBLUE, "* Вы хотите уволиться? (введите /accept quit, чтобы продолжить)");
	    WantsQuit[playerid] = true;
	}
	return 1;
}

CMD:accept(playerid, params[])
{
	new objective[32];
	if(sscanf(params,"s[32]", objective)) return SendClientMessage(playerid, COLOR_WHITE, "USAGE: /accept [job]");
	{
	    if(!strcmp(objective, "job", true))
	    {
	        if(WantsJob[playerid])
	        {
	            SendClientMessage(playerid, COLOR_LIGHTBLUE, "* Теперь вы пилот. Используйте /pilothelp, чтобы узнать команды.");
	            SetPlayerSkin(playerid, 61);
				HasPilotJob[playerid] = true;
				WantsJob[playerid] = false;
			}
			else
			{
			    SendClientMessage(playerid, COLOR_GRAD2, "Вам не предлагали устроиться на работу.");
			}
		}
		if(!strcmp(objective,"quit",true))
		{
		    if(WantsQuit[playerid])
		    {
		        SendClientMessage(playerid, COLOR_LIGHTBLUE, "* Вы уволились с работы.");
		        HasPilotJob[playerid] = false;
	    		WantsQuit[playerid] = false;
		        for(new i = 0; i < 5; i++) { PilotJobs[playerid][i] = false; }
		        DisablePlayerCheckpoint(playerid);
			}
			else
			{
			    SendClientMessage(playerid, COLOR_GRAD2, "Вы не собирались увольняться.");
			}
		}
	}
	return 1;
}

CMD:pilothelp(playerid)
	return ShowPlayerDialog(playerid, 9852, DIALOG_STYLE_MSGBOX, "Команды пилота", "{00FF00}/work {0000FF}-> {FFFFFF}Начать работу.\n{00FF00}/quitjob {0000FF}-> {FFFFFF}Уволиться.\n{00FF00}/accept [job/quit] {0000FF}-> {FFFFFF}Подтердить устройство/увольнение с работы.\n{00FF00}/join {0000FF}-> {FFFFFF}Устроиться на работу пилотом.\n{00FF00}/pilothelp {0000FF}-> {FFFFFF}Показать эту справку.", "Ок", "");

CMD:work(playerid)
{
	new vehicleid = GetPlayerVehicleID(playerid);
	if(HasPilotJob[playerid] == false) return SendClientMessage(playerid, COLOR_GRAD2, "Вы не пилот.");
	if(IsPlayerInJob[playerid]) return SendClientMessage(playerid, COLOR_GRAD2, "Сначала доставьте свой груз!");
	if(GetVehicleModel(vehicleid) != 519 && GetVehicleModel(vehicleid) != 487 && GetVehicleModel(vehicleid) != 417 && GetVehicleModel(vehicleid) != 593) return SendClientMessage(playerid, COLOR_GRAD2, "Вы не в самолёте.");
	{
		GiveRandomJob(playerid);
	}
	return 1;
}

#endif