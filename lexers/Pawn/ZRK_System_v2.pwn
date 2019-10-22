// [Script] �������-�������� �������� "AirFest". (- ��������� ��������) ^
// ���� ������ ����������: 15.08.2014                                   ^
// ���� ������: 04/02/2015                                              ^
// �����: IceShock                                                      ^
// Developer Skype: ice_and_shock                                       ^
// Version: 2.0 RELEASE 			                                    ^
// ��� ����� ��������. (c) 2015 by IceShock.                            ^
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#include <a_samp> // SA:MP ����       //
#include <foreach> // Easy Cycles     //
#include <sscanf2> // Specifiers      //
#include <zcmd> // Command Engine     //
//#include <TestBotsAPI> // My pretty   //
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ //
#undef MAX_PLAYERS // �������� ��������� ��� � �������������� � ����� ���������� �������� ���� �������� ������.
const MAX_PLAYERS = 500; // ��������� ����� � ������� ��� �������, �.� - �������� 500 �� ���� ���-�� ������ �������.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
#define public: public // ���� �� ������ ����������� ���� �������� �� �������� ���� ��������
#define SCM SendClientMessage // ����������
#define SPVi SetPVarInt // ����������
#define GPVi GetPVarInt // ����������
#define pSPVi(%0,%1,%2); \
SetPVarInt(%0,%1,(GetPVarInt(%0,%1) + %2));
#define mSPVi(%0,%1,%2); \
SetPVarInt(%0,%1,(GetPVarInt(%0,%1) - %2));
//&&&&&&&&&&&&&&&&& ���������� ������������ ����� �������-��������� ��������� //
#define bp_X (308.1761) // x ���������� ������ � ������������
#define bp_Y (1976.6831) // y ���������� ������ � ������������
#define bp_Z (18.0774) // z ���������� ������ � ������������
#define bp_A (178.3909) // ������� ���������� ������ � ������������
//----------------//
#define rad_X (307.8577) // x ���������� ������ � �������
#define rad_Y (1956.1046) // y ���������� ������ � �������
#define rad_Z (18.2654) // z ���������� ������ � �������
#define rad_A (179.2643) // ������� ���������� ������ � �������
//----------------//
#define zur_X (307.6257) // x ���������� ������ � ����������
#define zur_Y (1936.5151) // y ���������� ������ � ����������
#define zur_Z (18.2910) // z ���������� ������ � ����������
#define zur_A (179.0865) // ������� ���������� ������ � ����������
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#define chX (276.8094) // x ���������� ��������� �������� ����������
#define chY (1936.2177) // y ���������� ��������� �������� ����������
#define chZ (18.0773) // z ���������� ��������� �������� ����������
//----------------------------------------------------------------------------//
#define BEZHEVOY 0xF5DEB3AA///////////////////////////////////////
#define SEAGREEN 0x00EEADDF///////////////////////////////////////
#define RED 0xAA3333AA////////////////////////////////////////////
#define GREY 0xAFAFAFAA///////////////////////////////////////////
#define YELLOW 0xFFFF00AA/////////////////////////////////////////
#define PINK 0xFF66FFAA///////////////////////////////////////////
#define BLUE 0x0000BBAA///////////////////////////////////////////
#define WHITE 0xFFFFFFAA//////////////////////////////////////////
#define ORANGE 0xFF9900AA/////////////////////////////////////////
#define LIGHTRED 0xFF99AADD///////////////////////////////////////
#define LIGHTGREEN 0x24FF0AB9/////////////////////////////////////
#define GREEN 0x33AA33AA//////////////////////////////////////////
#define LIGHTBLUE 0x33CCFFAA//////////////////////////////////////
//----------------------------- ������� � ���������� ---------------------------------------------------------------------//
new zcmd[128]; // ��� ����� ������ ������� ��������� ��������������, ���� �� ����� ���� � ���������� ����������� ���������.
new n_value = 0; // ��� ������� ���-�� ����������
new Missile = 0; // �������� ���������� � ���
new ZRK[3]; // ������ ���������
new OBJ[3]; // ������� �� ����������
new bool:EN = false; // For timer
new OON; // ��� �������
new r1[MAX_PLAYERS], r2[MAX_PLAYERS], r3[MAX_PLAYERS];
new bool:in_proccess[MAX_PLAYERS]; // anti repeat fire
new bool:RadActive[MAX_PLAYERS]; // ������ ������ ������ ���������
new bool:RangMode = false; // �������� ������ �� �� ��� �� �������� �������� �� �������� ����� ����� PVar ���� ����� ����
new bool:FreeMats = true; // ���������� ��������� �������� ��, ������ false ���� ������ ��������� ���� � ���� � �������.
// �������� � ���������� RangMode �������� false �� true ���� �� ������ ��������� �� ������� �� ������� ������
new ArmySkins[6] = {287, 191, 179, 61, 255}; // ����� ���� ������ ������� �������� ����������� ������. (���� ��� ����� ������ �� ������ �� ���� �����)
// ��� ����� ����� 5 ������� ������. ��� ��� ���� �������� �� �������� ����.
/*
ID Skina 287 - ����������� ���� ������� � ������� ����� � ������������ � ������ (������� ���)
ID Skina 191 - ����������� ���� ������� � ������� ����� � ������ (������� ���)
ID Skina 179 - ����� � ������, ������ ��� �� ������ ��� �������� ������ � ���� (������� ���)
ID Skina 61 - ���� ������ � �������� ����� ������� � ������ ����� ����� � ���������� (������� ���)
ID Skina 255 - ���� ������� ������ � ������� � ������ ������ (��� ������� �� �������) (������� ���)
*/
// ------------- ��������� ------------------------------------------------------------------------------------//
const MAX_MISSILES = 4; // �������� ����������� �������� ���������� � �������� ��������� ���.
const MAX_CARGO = 12; // �������� ����������� �������� ���������� � �������� ���.
const MATS_PRICE = 4000; // ���� �� �������� ����� � ����������
const Float:MAX_RAD_DISTANCE = 3000.0; // ������������ ������ �������� ��� - 3 ��.
// :::::::::::::::::::::: ������ ������ ������� ������� � �� � ������ ���� :::::::::: //
/* // ��� ��� ����� ������ ����� ���� �� �������� �������� true � ���������� RangMode, ����� ��� �� ����� ���� ���� ������ ���.
� public OnPlayerSpawn ��������� ��� ������� if, ��������� �� �����, �.� ��� ����� ���� ������ � ���������� ��� �����.
{
	if(PlayerInfo[playerid][pMember] == 3) SetPVarInt(playerid, "z_MilitaryRang", (PlayerInfo[playerid][pRang])); // ����������
	return true;
}
//������ �������� ���������� � ���� � ��
// ��� ��� ����� ������ ����� ���� �� �������� �������� true � ���������� FreeMats, ����� ��� �� ����� ���� ���� ������ ��� ����.
//����������� �� �� �������� } � � ����� ����� ���� ��������� �� ������ ��� ��� ����,
//--------------------------//
#define n_varbie LV_Mats   // LV_Mats �������� �� ��� ����� ���������� ���������� ����� �����.
//-----------------------//
forward SendMatsValue();
public SendMatsValue()
{
	CallRemoteFunction("SetMatsValue", "d", n_varbie);
	return true;
}
forward TakeMatsValue(val);
public TakeMatsValue(val)
{
	n_varbie -= val;
	return true;
}
*/
//%%%%%%%%%%%%%%%%% Callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%//
public: OnFilterScriptInit()
{
	print("\n((((((((((((((((((((((((((()))))))))))))))))))))))))");
	print(" [Filter Script] ZRK System ""AirFest"" ------------- ");
	print("((((((((((((((((((((((((((((()))))))))))))))))))))))\n");
	//AllowBots();
	return true;
}
////////////////////////////////////////////////////////////////////////////////
public: OnGameModeInit()
{
    SetTimer("Fresher", 1000, true); // �� ��������� OnPlayerUpdate, ������� ���� ����� ����������� ������ � ������.
	AllowVehicles(); // ������� ��������� � ��.
	AllowObjects(); // ������� ������� � ��.
	return true;
}
public: OnPlayerConnect(playerid)
{
	WritePerVars(playerid); // ��������� ������� ���� ����� � ��.
	return true;
}
public: OnPlayerDisconnect(playerid, reason)
{
    if(GPVi(playerid, "z_Timer") > 0) KillTimer(OON);
	return true;
}
//-------------------------------------------------//
IsClientMilitary(playerid) // ������� ��� �����������, ��������� �� ������ �������.
{
	if(RangMode == true)
	{
		if(GPVi(playerid, "z_MilitaryRang") > 0) return true; // ���� ���� ������ ����, �� �������� ������.
	}
	else if(RangMode == false)
	{
		for(new id = 0; id < 6; id++)
		{
		    if(GetPlayerSkin(playerid) == ArmySkins[id]) return true;
		}
	}
	return false;
}
#define fclamp360(%0) %0 += (%0 >= 360.0) ? -360.0 : ((%0 < 0.0) ? 360.0 : 0.0) // Trigonometric Macro
Float:ReturnZAngleToPoint(vehicleid, Float:x, Float:y) // Calculating the angle
{
    new Float:P[4];
    GetVehiclePos(vehicleid, P[0], P[1], P[2]);
    P[3] = (180.0 - atan2(floatsub(P[0], x), floatsub(P[1], y)));
    fclamp360(P[3]);
    return P[3];
}
Cargo(playerid) // ������� ��� �������� ������� ����� �������.
{
	if(GPVi(playerid, "z_Cargo") == 0) return false; // ���� ��� ����� �� ������ �� ��������.
	return true;
}
public: OnPlayerSpawn(playerid)
{
	if(GPVi(playerid, "z_Spawned") == 0) SPVi(playerid, "z_Spawned", 1); // ���� �������� ������� ������, ��������� ���� �������� �� ���������.
	//PutBots(playerid);
	return true;
}
public: OnPlayerExitVehicle(playerid, vehicleid)
{
	if(vehicleid == ZRK[1] && RadActive[playerid] == true)
	{
		RadActive[playerid] = false; KillTimer(OON); SPVi(playerid, "z_Timer", 0);
		SCM(playerid, RED, "������������ ��������.");
		foreach(new i : Player) SPVi(i, "z_Target", 0);
	}
	return true;
}
public: OnPlayerStateChange(playerid, newstate, oldstate)
{
    if(newstate == PLAYER_STATE_DRIVER)
	{
	    new newcar = GetPlayerVehicleID(playerid);
		if(newcar >= ZRK[0] && newcar <= ZRK[2])
		{
		    if(IsClientMilitary(playerid)){}
		    else
			{
			    RemovePlayerFromVehicle(playerid);
			    SCM(playerid, GREEN, "�� �� ������!");
			    return true;
			}
		}
		if(newcar == ZRK[0] && IsClientMilitary(playerid) && !Cargo(playerid))
		{
		    SCM(playerid, ORANGE, "������� /tocargo ����� ������ �������� �������� ����������.");
		}
		if(newcar == ZRK[1] && IsClientMilitary(playerid))
		{
		    SCM(playerid, PINK, "����� ���������� �� ������� ����������� � ������������ �����!");
		    SCM(playerid, PINK, "������� /airscan ����� ������ ����������� ����!");
		}
	}
	return true;
}
public: OnPlayerEnterRaceCheckpoint(playerid)
{
	if(IsPlayerInRangeOfPoint(playerid, 5.0, chX, chY, chZ))
	{
	    new newcar = GetPlayerVehicleID(playerid);
	    if(!Cargo(playerid) && newcar == ZRK[0] && IsClientMilitary(playerid))
	    {
	        if(FreeMats == false && GiveMatsValue() < MATS_PRICE) SCM(playerid, RED, "������������ ���������� ��� ������� ����������!");
	        else
	        {
	            if(FreeMats == false) TakeMats(MATS_PRICE);
	            TogglePlayerControllable(playerid, false);
	            SCM(playerid, YELLOW, "��� ��������, ���������� ���������!");
	            PlayerPlaySound(playerid, 6802, 0.0, 0.0, 0.0);
	            SetTimerEx("MatsHaul", 5000, false, "i", playerid);
	        }
	        DisablePlayerRaceCheckpoint(playerid);
	    }
	}
	return true;
}
GetPlayerSpeed(playerid)
{
    new Float:ST[4];
    if(IsPlayerInAnyVehicle(playerid))GetVehicleVelocity(GetPlayerVehicleID(playerid),ST[0],ST[1],ST[2]);
    else GetPlayerVelocity(playerid,ST[0],ST[1],ST[2]);
    ST[3] = floatsqroot(floatpower(floatabs(ST[0]), 2.0) + floatpower(floatabs(ST[1]), 2.0) + floatpower(floatabs(ST[2]), 2.0)) * 150.0;
    return floatround(ST[3]);
}
Float:GetDistanceBetweenPlayers(p1, p2)
{
	new Float:x1,Float:y1, Float:z1, Float:x2, Float:y2, Float:z2;
	if(!IsPlayerConnected(p1) || !IsPlayerConnected(p2))
	{
		return -1.00;
	}
	GetPlayerPos(p1,x1,y1,z1);
	GetPlayerPos(p2,x2,y2,z2);
	return floatsqroot(floatpower(floatabs(floatsub(x2,x1)), 2) + floatpower(floatabs(floatsub(y2 ,y1)), 2) + floatpower(floatabs(floatsub(z2, z1)), 2));
}
forward MatsHaul(playerid);
public MatsHaul(playerid)
{
	new sabotage[37+1], bName[MAX_PLAYER_NAME];
	GetPlayerName(playerid, bName, sizeof(bName));
	format(sabotage, sizeof(sabotage), "���� %s �������� �������� ����������.", bName);
    foreach(new x : Player)
	if(GetDistanceBetweenPlayers(playerid, x) <= 30.0) SCM(x, LIGHTGREEN, sabotage);
    SCM(playerid, LIGHTGREEN, "�������� � ���������� ������� ��������� � ������� ����� /preload.");
    TogglePlayerControllable(playerid, true);
    SPVi(playerid, "z_Cargo", 12);
    PlayerPlaySound(playerid, 5201, 0.0, 0.0, 0.0);
	return true;
}
forward Preload(playerid);
public Preload(playerid)
{
    new sabotage[60+1], bName[MAX_PLAYER_NAME];
	GetPlayerName(playerid, bName, sizeof(bName));
	format(sabotage, sizeof(sabotage), "���� %s �������� ��������� �������� ����������.", bName);
    foreach(new x : Player)
    {
		if(GetDistanceBetweenPlayers(playerid, x) <= 30.0) SCM(x, GREEN, sabotage);
	    if(IsClientMilitary(x) && IsPlayerInAnyVehicle(x))
		{
		    if(GetPlayerVehicleID(x) == ZRK[2]) SCM(x, SEAGREEN, "�������-�������� ��������� ��������! ���-��: 4 ����������.");
			break;
		}
	}
    TogglePlayerControllable(playerid, true);
    SPVi(playerid, "z_Cargo", 0); Missile = 4;
    PlayerPlaySound(playerid, 5201, 0.0, 0.0, 0.0);
	return true;
}
forward RadarSystem(playerid);
public RadarSystem(playerid)
{
	foreach(new target : Player)
	{
	    //~~~~~~~~~~~~~~~~~~~~~~ Radar SyS ----------------------- //
	    new Float:n_X, Float:n_Y, Float:n_Z; GetVehiclePos(ZRK[2], n_X, n_Y, n_Z);
	    new Float:X, Float:Y, Float:Z; GetPlayerPos(target, X, Y, Z);
		if(IsPlayerInAnyVehicle(target) && RadActive[playerid] == true && Z > 50.0)
		{
			if(IsPlayerInRangeOfPoint(target, MAX_RAD_DISTANCE, n_X, n_Y, n_Z))
			{
			    new cll = class(target);
			    if(cll != -1)
				{
				    if(GPVi(target, "z_Target") == 0)
				    {
					    SPVi(target, "z_Target", 1);
					    new Radar[1+110];
					    new classification[10];
						if(class(target) == 1) classification = "������";
						else if(class(target) == 2) classification = "�������";
						format(Radar, sizeof(Radar),
						"���������� ����! ID:%d | �������������: %s | \
						��������: %d/kmh | ������: %d m | ���������: %d m",
					    target, classification, GetPlayerSpeed(target), floatround(Z), floatround(GetDistanceBetweenPlayers(playerid, target)));
						PlayerPlaySound(playerid, 5201, 0.0, 0.0, 0.0);
						SendClientMessage(playerid, GREEN, Radar);
						SCM(playerid, SEAGREEN, "������� /targetsend ����� ���� �������.");
				    }
				}
				else if(!IsPlayerInRangeOfPoint(target, MAX_RAD_DISTANCE, n_X, n_Y, n_Z))
				{
				    if(GPVi(target, "z_Target") == 1)
				    {
				        new contact[46+1];
				        format(contact, sizeof(contact), "���� %d:ID �������� ������ �������� ���������.", target);
				        SCM(playerid, LIGHTGREEN, contact); PlayerPlaySound(playerid, 41603, 0.0, 0.0, 0.0);
				        SPVi(target, "z_Target", 0);
				    }
				}
			}
		}
	}
	return true;
}
forward Zahvat(playerid, tid);
public Zahvat(playerid, tid)
{

	SCM(playerid, LIGHTRED, "���� ���������! �������� LMB ��� CTRL ��� �����.\n\
	��� �� ������� /canceltarget ��� ������ ����.");
	PlayerPlaySound(playerid, 21000, 0.0, 0.0, 0.0);
	SPVi(playerid, "z_Zahvat", tid); 
	return true;
}
public: OnPlayerKeyStateChange(playerid, newkeys, oldkeys)
{
	if(newkeys & KEY_FIRE)
	{
	    new newcar = GetPlayerVehicleID(playerid);
	    new target = GPVi(playerid, "z_Zahvat");
	    if(in_proccess[target] == true)
		{
	 		if(GPVi(playerid, "z_AntiFlood") == 0)
	 		{
			 	SCM(playerid, LIGHTRED, "�� ��� ��������� ���� �� ���� ����, �������� ����������!");
			 	pSPVi(playerid, "z_AntiFlood", 5);
			}
	 		return true;
		}
	    if(GPVi(playerid, "z_Zahvat") != -1 && newcar == ZRK[2]) Launch(playerid, target);
	}
	return true;
}
Launch(playerid, target)
{
	if(Missile == 0)
	{
	    SPVi(playerid, "z_Zahvat", -1);
		return SCM(playerid, PINK, "��������� ���������!");
	}
	in_proccess[target] = true;
	//new ref[7];
	//GetVehicleParamsEx(GetPlayerVehicleID(playerid), ref[0], ref[1], ref[2], ref[3], ref[4], ref[5], ref[6]);
	//SetVehicleParamsEx(GetPlayerVehicleID(playerid), 0, ref[1], ref[2], ref[3], ref[4], ref[5], ref[6]);
	new Float:xay, Float:yay, Float:zay;
	GetVehiclePos(GetPlayerVehicleID(playerid), xay, yay, zay);
	//(GetPlayerVehicleID(playerid), xay, yay, zay);
	#pragma unused zay
    DestroyObject(OBJ[1]);
    OBJ[1] = CreateObject(3267,0,0,-1000,0,0,0,100);
	AttachObjectToVehicle(OBJ[1],
	ZRK[2], 0.000000,-1.200000,0.824999,0.000000,0.000000,ReturnZAngleToPoint(GetPlayerVehicleID(target), xay, yay));
	foreach(new x : Player)
	if(GetDistanceBetweenPlayers(playerid, x) <= 30.0) PlayerPlaySound(x, 40408, 0.0, 0.0, 0.0);
	new Float:X, Float:Y, Float:Z; GetVehiclePos(ZRK[2], X, Y, Z);
	OBJ[2] = CreateObject(354,X,Y,(Z+4.0),0.0,0.0,0.0,200.0); Missile -= 1;
	SetTimerEx("MoveRocket", 100, false, "ii", playerid, target);
	return true;
}
Float:GetDistanceBetweenPoints(Float:X, Float:Y, Float:Z, Float:PointX, Float:PointY, Float:PointZ)
{
	return floatsqroot(floatadd(floatadd(floatpower(floatsub(X, PointX), 2.0), floatpower(floatsub(Y, PointY), 2.0)), floatpower(floatsub(Z, PointZ), 2.0)));
}
forward MoveRocket(playerid, target);
public MoveRocket(playerid, target)
{
	EN = true; new Float:DETONATION = 8.0;
	new Float:x, Float:y, Float:z, Float:X, Float:Y, Float:Z;
	if(IsPlayerInAnyVehicle(target))
	{
		new tg = GetPlayerVehicleID(target);
		GetVehiclePos(tg, x, y, z);
	}
	new Float:xX, Float:yY, Float:zZ; GetVehiclePos(ZRK[2], xX, yY, zZ);
	GetObjectPos(OBJ[2], X, Y, Z); new Float:DST = GetDistanceBetweenPoints(x, y, z, xX, yY, zZ);
	if(GetDistanceBetweenPoints(X, Y, Z, xX, yY, zZ) < 50.0)
	{
		MoveObject(OBJ[2], x, y, zZ, 100.0);
		SetTimerEx("MoveRocket", 100, false, "ii", playerid, target);
		return true;
	}
	if(GetPlayerSpeed(target) < 180 && DST < 1000.0) MoveObject(OBJ[2], x, y, z, 100.0);
	else if(GetPlayerSpeed(target) < 180 && DST > 1000.0) MoveObject(OBJ[2], x, y, z, 150.0);
	else if(GetPlayerSpeed(target) > 180 && DST < 1000.0) MoveObject(OBJ[2], x, y, z, 200.0);
	else if(GetPlayerSpeed(target) > 180 && DST > 1000.0){MoveObject(OBJ[2], x, y, z, 300.0); DETONATION = 10.0;}
    if(GetDistanceBetweenPoints(x, y, z, X, Y, Z) < DETONATION)
	{
		DestroyObject(OBJ[2]);
	    CreateExplosion(x, y, z, 10, 12.0);
		SPVi(playerid, "z_Zahvat", -1);
		in_proccess[target] = false;
		foreach(new radarshik : Player)
		{
		    if(IsPlayerInAnyVehicle(radarshik))
		    {
		        new newcar = GetPlayerVehicleID(radarshik);
			    if(newcar == ZRK[1])
				{
					SPVi(radarshik, "z_Sended", -1);
					break;
				}
			}
		}
		SPVi(target, "z_Target", 0);
		SCM(playerid, LIGHTBLUE, "���� ��������!");
		PlayerPlaySound(playerid, 5201, 0.0, 0.0, 0.0);
		DestroyObject(OBJ[1]);
	    OBJ[1] = CreateObject(3267,0,0,-1000,0,0,0,100);
		AttachObjectToVehicle(OBJ[1],
		ZRK[2], 0.000000,-1.200000,0.824999,0.000000,0.000000,0.000000);
		return true;
	}
	if(!IsPlayerConnected(target))
	{
	    SCM(playerid, ORANGE, "���� ���� �������� ������, ������ ���������������.");
	    PlayerPlaySound(playerid, 21001, 0.0, 0.0, 0.0);
	    DestroyObject(OBJ[2]);
	    SPVi(playerid, "z_Zahvat", -1);
	    in_proccess[target] = false;
	    DestroyObject(OBJ[1]);
	    OBJ[1] = CreateObject(3267,0,0,-1000,0,0,0,100);
		AttachObjectToVehicle(OBJ[1],
		ZRK[2], 0.000000,-1.200000,0.824999,0.000000,0.000000,0.000000);
	    foreach(new radarshik : Player)
		{
		    if(IsPlayerInAnyVehicle(radarshik))
		    {
		        new newcar = GetPlayerVehicleID(radarshik);
			    if(newcar == ZRK[1])
				{
					SPVi(radarshik, "z_Sended", -1);
					break;
				}
			}
		}
	    return true;
	}
	if(GetDistanceBetweenPoints(x, y, z, X, Y, Z) > MAX_RAD_DISTANCE)
	{
	    SCM(playerid, ORANGE, "���� ���� ��� ������������, ������ ���������������.");
	    PlayerPlaySound(playerid, 21001, 0.0, 0.0, 0.0);
	    DestroyObject(OBJ[2]);
	    SPVi(playerid, "z_Zahvat", -1);
	    in_proccess[target] = false;
	    DestroyObject(OBJ[1]);
	    OBJ[1] = CreateObject(3267,0,0,-1000,0,0,0,100);
		AttachObjectToVehicle(OBJ[1],
		ZRK[2], 0.000000,-1.200000,0.824999,0.000000,0.000000,0.000000);
	    foreach(new radarshik : Player)
		{
		    if(IsPlayerInAnyVehicle(radarshik))
		    {
		        new newcar = GetPlayerVehicleID(radarshik);
			    if(newcar == ZRK[1])
				{
					SPVi(radarshik, "z_Sended", -1);
					break;
				}
			}
		}
	    return true;
	}
	if(EN == true) SetTimerEx("MoveRocket", 100, false, "ii", playerid, target);
	return true;
}
TakeMats(value) return CallRemoteFunction("TakeMatsValue", "d", value);
GiveMatsValue()
{
	CallRemoteFunction("SendMatsValue", "", "");
	return n_value;
}
forward SetMatsValue(val);
public SetMatsValue(val)
{
	n_value = val;
	return true;
}
class(playerid)
{
	new val = -1;
	new newcar = GetPlayerVehicleID(playerid);
	new model = GetVehicleModel(newcar);
	if(model == 460) val = 1; // Hydroplan Skeemer
	else if(model == 512) val = 1; // Cropduster
	else if(model == 513) val = 1; // Stuntplane
	else if(model == 593) val = 1; // Dodo
	else if(model == 511) val = 1; // Beagle
	else if(model == 519) val = 1; // Shamal
	else if(model == 577) val = 1; // AT-400
	else if(model == 553) val = 1; // Nevada
	else if(model == 592) val = 1; // Andromada
	else if(model == 476) val = 1; // Rustler Plane
	else if(model == 520) val = 1; // Hydra
	//------------------------------------------------//
	else if(model == 469) val = 2; // Sparrow
	else if(model == 487) val = 2; // Mavrick
	else if(model == 488) val = 2; // SAN News Maverick
	else if(model == 425) val = 2; // Hunter Helicopter
	else if(model == 417) val = 2; // Leviathan
	else if(model == 548) val = 2; // Cargobob
	else if(model == 563) val = 2; // Raindance
	else if(model == 447) val = 2; // Seasparrow
	else if(model == 497) val = 2; // Police Maverick
	return val;
}
//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& My Functions &&&&&&&&&&&&&&&&&&&&&&&&&&&//
forward Fresher();
public Fresher()
{
	foreach(new hallow : Player)
	{
	    if(GPVi(hallow, "z_Spawned") == 1)
	    {
			if(GPVi(hallow, "z_AntiFlood") > 0) mSPVi(hallow, "z_AntiFlood", 1);
	    }
	}
	return true;
}
//-------------------------------//
public: OnPlayerTakeDamage(playerid, issuerid, Float:amount, weaponid, bodypart) return true;
AllowVehicles()
{
	ZRK[0] = CreateVehicle(455, bp_X, bp_Y, bp_Z, bp_A, 120, 120, 60000); // ������ ���
	ZRK[1] = CreateVehicle(578, rad_X, rad_Y, rad_Z, rad_A, 120, 120, 60000); // ������ ��������
	ZRK[2] = CreateVehicle(573, zur_X, zur_Y, zur_Z, zur_A, 120, 120, 60000); // ������ ���
	return true;
}
AllowObjects()
{
	// ����� �� ������� ����������� � ������������ "�����"
    OBJ[0] = CreateObject(1596,0,0,-1000,0,0,0,100);
	AttachObjectToVehicle(OBJ[0],
	ZRK[1], 0.000000,-1.875000,2.400000,0.000000,0.000000,899.102722);
 	// ���������� ������� ��������� (���)
 	OBJ[1] = CreateObject(3267,0,0,-1000,0,0,0,100);
	AttachObjectToVehicle(OBJ[1],
	ZRK[2], 0.000000,-1.200000,0.824999,0.000000,0.000000,0.000000);
	return true;
}
public OnVehicleSpawn(vehicleid)
{
    if(vehicleid == ZRK[2])
    {
        DestroyObject(OBJ[1]);
        OBJ[1] = CreateObject(3267,0,0,-1000,0,0,0,100);
		AttachObjectToVehicle(OBJ[1],
		ZRK[2], 0.000000,-1.200000,0.824999,0.000000,0.000000,0.000000);
    }
    return 1;
}
//------------------------------------------------------------------//
WritePerVars(playerid) // Hear the NULL is a false result of var
{
	// Creating Intermediate PVars
	SPVi(playerid, "z_MilitaryRang", 0); // ���� � �����
	SPVi(playerid, "z_Spawned", 0); // �� ���������.
	SPVi(playerid, "z_Cargo", 0); // ����.
	SPVi(playerid, "z_AntiFlood", 0); // ���� ����
	SPVi(playerid, "z_Target", 0); // Targeting
	SPVi(playerid, "z_Timer", 0); // For radar timer
	SPVi(playerid, "z_Zahvat", -1); // Target
	SPVi(playerid, "z_Sended", -1); // Target ID
	r1[playerid] = -1; r2[playerid] = -1; r3[playerid] = -1;
	RadActive[playerid] = false; in_proccess[playerid] = false;
	return true;
}
//---------------------------------- ZCMD - ��������� ���� ------------------------------------------------------- //
CMD:tocargo(playerid, params[], help)
{
    if(GPVi(playerid, "z_Spawned") == 0) return true; // ���� �� �� ������������ �� ���������� ���������� �������.
    if(!IsClientMilitary(playerid)) return true; // ���� �� �� ����� �� �� ��������� ��� ����.
    if(!IsPlayerInAnyVehicle(playerid)) return true;
    if(!Cargo(playerid))
    {
        if(GPVi(playerid, "z_AntiFlood") > 0) return SCM(playerid, BEZHEVOY, "�� ��� ������������!");
	    new newcar = GetPlayerVehicleID(playerid);
	    if(newcar != ZRK[0]||GetPlayerVehicleSeat(playerid) != 0) return SCM(playerid, GREEN, "�� ������ ������ �� ���� ������ ���!");
	    SetPlayerRaceCheckpoint(playerid, 1, chX, chY, chZ, chX, chY, chZ, 5.0);
		SCM(playerid, LIGHTRED, "�������� � ������� �� ���������.");
		SPVi(playerid, "z_AntiFlood", 5);
	}
	else return SCM(playerid, GREEN, "�� ��� �����������!");
	return true;
}
/*CMD:hidedraws(playerid, params[], help)
{
	CallRemoteFunction("_FS_HidePlayerDraws", "i", playerid);
	return true;
}
CMD:tobase(playerid, params[], help)
{
    if(GPVi(playerid, "z_Spawned") == 0) return true; // ���� �� �� ������������ �� ���������� ���������� �������.
    if(!IsClientMilitary(playerid)) return true; // ���� �� �� ����� �� �� ��������� ��� ����.
    SetPlayerPos(playerid, chX + 10.0, chY, chZ);
    SCM(playerid, YELLOW, "In base!");
	return true;
}
CMD:botlaunch(playerid, params[], help)
{
    if(GPVi(playerid, "z_Spawned") == 0) return true; // ���� �� �� ������������ �� ���������� ���������� �������.
    if(!IsClientMilitary(playerid)) return true; // ���� �� �� ����� �� �� ��������� ��� ����.
    Launch(playerid, botid);
    SCM(playerid, YELLOW, "Launch!");
	return true;
}
CMD:melaunch(playerid, params[], help)
{
    if(GPVi(playerid, "z_Spawned") == 0) return true; // ���� �� �� ������������ �� ���������� ���������� �������.
    if(!IsClientMilitary(playerid)) return true; // ���� �� �� ����� �� �� ��������� ��� ����.
    Launch(playerid, playerid);
    SCM(playerid, YELLOW, "Launch!");
	return true;
}
CMD:reload(playerid, params[], help)
{
    if(GPVi(playerid, "z_Spawned") == 0) return true; // ���� �� �� ������������ �� ���������� ���������� �������.
    if(!IsClientMilitary(playerid)) return true; // ���� �� �� ����� �� �� ��������� ��� ����.
    Missile = 4;
    SCM(playerid, YELLOW, "Reloaded!");
	return true;
}*/
CMD:airscan(playerid, params[], help)
{
    if(GPVi(playerid, "z_Spawned") == 0) return true; // ���� �� �� ������������ �� ���������� ���������� �������.
    if(!IsClientMilitary(playerid)) return true; // ���� �� �� ����� �� �� ��������� ��� ����.
    new newcar = GetPlayerVehicleID(playerid);
    if(!IsPlayerInAnyVehicle(playerid)||newcar != ZRK[1]) return true;
    if(RadActive[playerid] == true) return SCM(playerid, BEZHEVOY, "�� ��� ������������ ����!");
    SCM(playerid, RED, "������������ ������!"); RadActive[playerid] = true;
    OON = SetTimerEx("RadarSystem", 1000, true, "i", playerid); SPVi(playerid, "z_Timer", 1);
	return true;
}
CMD:targetsend(playerid, params[], help)
{
    if(GPVi(playerid, "z_Spawned") == 0) return true; // ���� �� �� ������������ �� ���������� ���������� �������.
    if(!IsClientMilitary(playerid)) return true; // ���� �� �� ����� �� �� ��������� ��� ����.
    new newcar = GetPlayerVehicleID(playerid);
    if(GPVi(playerid, "z_AntiFlood") > 0) return SCM(playerid, BEZHEVOY, "�� ��� ���������� ����, ���������.");
    if(!IsPlayerInAnyVehicle(playerid)||newcar != ZRK[1]) return true;
    if(sscanf(params, "ud", params[0], params[1])) return SCM(playerid, WHITE, "������� /targetsend [ID ���������] [ID ����]");
    if(GPVi(params[1], "z_Target") != 1) return SCM(playerid, BEZHEVOY, "�� �� ������������ ����� ����!");
    if(GPVi(params[0], "z_Spawned") == 0) return SCM(playerid, BEZHEVOY, "���� ����� �� �����������!");
    if(GPVi(playerid, "z_Sended") == params[1]) return SCM(playerid, BEZHEVOY, "�� ��� ���������� ������� �� ��� ����!");
    if(GetDistanceBetweenPlayers(playerid, params[0]) > 20.0) return SCM(playerid, BEZHEVOY, "��������� ����� ���� ������ ���� �� ����� 20 ������!");
    new necar = GetPlayerVehicleID(params[0]);
	if(!IsPlayerInAnyVehicle(playerid)||necar != ZRK[2]) return SCM(playerid, YELLOW, "����� ������ ���� � ���������� ������� ���������!");
	r1[params[0]] = random(1000); r2[params[0]] = random(1000); r3[params[0]] = random(1000);
	format(zcmd, sizeof(zcmd), "[������� �����]: ������� �� ���� %d:ID\n\
	{00CCFF}��� ������� ���� ������� /azimut %d %d %d %d", params[0], params[1], r1[params[0]], r2[params[0]], r3[params[0]]);
	SCM(params[0], YELLOW, zcmd); SCM(playerid, YELLOW, "������������ ���������� ���������.");
	SPVi(playerid, "z_AntiFlood", 3); SPVi(playerid, "z_Sended", params[1]);
	return true;
}
CMD:azimut(playerid, params[], help)
{
    if(GPVi(playerid, "z_Spawned") == 0) return true; // ���� �� �� ������������ �� ���������� ���������� �������.
    if(!IsClientMilitary(playerid)) return true; // ���� �� �� ����� �� �� ��������� ��� ����.
    new newcar = GetPlayerVehicleID(playerid);
    if(GPVi(playerid, "z_Zahvat") == 1) return SCM(playerid, BEZHEVOY, "�� ��� ��������� ����!");
    if(GPVi(playerid, "z_AntiFlood") > 0) return SCM(playerid, BEZHEVOY, "�� ��� ������������ ����!");
    if(!IsPlayerInAnyVehicle(playerid)||newcar != ZRK[2]) return true;
    if(sscanf(params, "uddd", params[0], params[1], params[2], params[3]))
	{
		return SCM(playerid, WHITE, "������� /azimut [ID ����] [��� 1] [��� 2] [��� 3]");
	}
	if(GPVi(params[0], "z_Target") != 1)
	{
		return SCM(playerid, BEZHEVOY, "��� �� ��������� ������� ����� ����!");
	}
	if(params[1] != r1[playerid]||params[2] != r2[playerid]||params[3] != r3[playerid])
	{
		return SCM(playerid, BEZHEVOY, "�������� �����������!");
	}
    if(GPVi(params[0], "z_Spawned") == 0)
	{
		return SCM(playerid, BEZHEVOY, "���� ����� �� �����������!");
	}
	if(GetDistanceBetweenPlayers(playerid, params[0]) > MAX_RAD_DISTANCE) return SCM(playerid, BEZHEVOY, "���� ��� ������������ ���������!");
	SCM(playerid, GREEN, "������ ����..."); SetTimerEx("Zahvat", 3000, false, "ii", playerid, params[0]);
	SPVi(playerid, "z_AntiFlood", 3); 
	return true;
}
CMD:canceltarget(playerid, params[], help)
{
    if(GPVi(playerid, "z_Spawned") == 0) return true; // ���� �� �� ������������ �� ���������� ���������� �������.
    if(!IsClientMilitary(playerid)) return true; // ���� �� �� ����� �� �� ��������� ��� ����.
    new newcar = GetPlayerVehicleID(playerid);
    if(!IsPlayerInAnyVehicle(playerid)||newcar != ZRK[2]) return SCM(playerid, BEZHEVOY, "�� �� � ���������� ������� ���������!");
    if(GPVi(playerid, "z_Zahvat") == 0) return SCM(playerid, BEZHEVOY, "�� �� ����������� ����!");
	SCM(playerid, BLUE, "������ ������."); SPVi(playerid, "z_Zahvat", -1);
	return true;
}
CMD:preload(playerid, params[], help)
{
    if(GPVi(playerid, "z_Spawned") == 0) return true; // ���� �� �� ������������ �� ���������� ���������� �������.
    if(!IsClientMilitary(playerid)) return true; // ���� �� �� ����� �� �� ��������� ��� ����.
    if(!Cargo(playerid)) return SCM(playerid, BEZHEVOY, "�� �� ��������� ���� �������!");
    if(GPVi(playerid, "z_AntiFlood") > 0) return SCM(playerid, BEZHEVOY, "�� ��� ���������� ���������!");
    new newcar = GetPlayerVehicleID(playerid);
    if(!IsPlayerInAnyVehicle(playerid)||newcar != ZRK[0]) return SCM(playerid, BEZHEVOY, "�� �� � ������ ���!");
	foreach(new x : Player)
	{
	    if(IsPlayerInAnyVehicle(x))
	    {
      		new necar = GetPlayerVehicleID(x);
		    if(necar == ZRK[2])
		    {
				if(GetDistanceBetweenPlayers(playerid, x) < 20.0)
				{
				    if(Missile == 4) return SCM(playerid, RED, "��������� � ��� ��������� ��������!");
				    new sabotage[34+1], bName[MAX_PLAYER_NAME];
					GetPlayerName(playerid, bName, sizeof(bName));
					format(sabotage, sizeof(sabotage), "���� %s ����� �������� ����������.", bName);
				    foreach(new xx : Player)
					if(GetDistanceBetweenPlayers(playerid, xx) <= 30.0) SCM(x, LIGHTGREEN, sabotage);
				    SetTimerEx("Preload", 5000, false, "i", playerid);
					SCM(playerid, PINK, "��� �������� � ���������, ���������...");
					SPVi(playerid, "z_AntiFlood", 5);
					TogglePlayerControllable(playerid, false);
					break;
				}
				else
				{
					SCM(playerid, GREY, "�� ������ ���� � ������� 20 ������ �� ���������!");
					return true;
				}
			}
		}
	}
	return true;
}
//^^^^^^^^^^^^^^^^^^^^^^^^^_____________HAPPY * AND ~!!!!!!!^^^^^^^^^^^^^^^^^^||
