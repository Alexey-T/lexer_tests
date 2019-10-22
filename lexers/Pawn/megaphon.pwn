/* 
																	Megaphone Filterscript

					The megaphone filterscript was scripted by Rehasher, credits must not be removed from the filterscript.
					The filterscript is licensed under the Don't be a dick public license (DBAD). http://www.dbad-license.org/ Everyone is permitted to copy and distribute verbatim or modified copies of this license document, and changing it is allowed as long as the name is changed.

*/


#include <a_samp>
#include <zcmd>
#include <sscanf2>
#include <foreach>

// ==================================
// |			ARRAY				|
// |	Includes sound ids and		|
// |	voice descriptions.			|
// ==================================


new MegaphoneSounds[][] =
{
	// SPLIT COMMA
	// SOUND ID , DESCRIPTION
	{"9605,Give up. You're surrounded!"},
	{"9612,We know you're in there!"},
	{"10200,Hey you! Police. Stop!"},
	{"15800,This is the Los Santos Police Department; Stay where you are!"},
	{"15801,Freeze! Or we will open fire"},
	{"15802,Go! Go! Go!"},
	{"34402,Police! Don't move!"},
	{"34403,Get outta the car with your hands in the air!"},
	{"15825,LSPD. Stop right... are you insane? You'll kill us all!"}
};

// ==================================
// |			DEFINITIONS			|
// | Colours, dialogs and more...   |
// ==================================


#define DIALOG_MEGAPHONE_MENU 		(10201)

#define COLOR_WHITE					0xFFFFFFFF
#define COLOR_LIGHTBLUE				0x0080FFFF
#define COLOR_RED                   0xAA3333AA
#define VERSION						("1.1")

#define SCM SendClientMessage

// ==================================
// |			VARIABLES			|
// | Used for stuff, many stuff	    |
// ==================================

new Message[1000];

// ==================================
// |			CALLBACKS			|
// | Used for stuff, many stuff	    |
// ==================================

public OnFilterScriptInit()
{
	printf("   "); printf("   "); printf("   ");

	print("Megaphone Filterscript 			   			   ");
	print("Filtescript was successfully loaded.            ");
	printf("Filterscript Version : %s 				       ", VERSION);

	printf("   "); printf("   "); printf("   ");
	return 1;
}

public OnDialogResponse(playerid, dialogid, response, listitem, inputtext[])
{
	if(dialogid == DIALOG_MEGAPHONE_MENU)
	{
		if(!response) return SCM(playerid, COLOR_WHITE, "Cancel");
		new soundid, tw1 = 0;
        for(new w1 = 0; w1 < sizeof(MegaphoneSounds); w1++)
        {
            if(tw1 != listitem)
            {
                tw1++;
                continue;
            }
            new tmp1[2][128];
            split(MegaphoneSounds[w1], tmp1, ',');
            soundid = strval(tmp1[0]);
            break;
        }
        new Float:pos[4];
        GetPlayerPos(playerid, pos[0], pos[1], pos[2]);
        PlaySoundEx(soundid, pos[0], pos[1], pos[2], 15);
        return 1;
	}
	return 0;
}

// ==================================
// |			COMMANDS			|
// | Used for stuff, many stuff	    |
// ==================================

CMD:megaphone(playerid, params[])
{
	if(!sscanf(params, "s[250]", params))
	{
		format(Message, sizeof(Message), "*MEGAPHONE* %s", params);
		ProxDetector(15.0, playerid, Message, COLOR_RED);
		return 1;
	}

	new str1[2500], c1 = 0;
    for(new w1 = 0; w1 < sizeof(MegaphoneSounds); w1++)
    {
        new tmp1[2][128];
        split(MegaphoneSounds[w1], tmp1, ',');
        if(c1 == 0) format(str1, sizeof(str1), "{FFFFFF}%s\n", tmp1[1]);
        if(c1 > 0) format(str1, sizeof(str1), "%s{FFFFFF}%s\n", str1, tmp1[1]);
        c1++;
    }
	ShowPlayerDialog(playerid, DIALOG_MEGAPHONE_MENU, DIALOG_STYLE_LIST, "Megaphone Menu", str1, "Play", "Cancel");
	return 1;
}

// ==================================
// |			STOCKS				|
// | Used for stuff, many stuff	    |
// ==================================

stock ProxDetector(Float:radi, playerid, string[], color)
{
    new Float:x,Float:y,Float:z;
    GetPlayerPos(playerid,x,y,z);
    foreach(Player,i)
    {
        if(IsPlayerInRangeOfPoint(i,radi,x,y,z)) 
        {
            SCM(i,color,string);
        }
    }
}

stock split(const strsrc[], strdest[][], delimiter)
{
    new i, li;
    new aNum;
    new len;
    while(i <= strlen(strsrc))
    {
        if(strsrc[i] == delimiter || i == strlen(strsrc))
        {
            len = strmid(strdest[aNum], strsrc, li, i, 128);
            strdest[aNum][len] = 0;
            li = i+1;
            aNum++;
        }
        i++;
    }
    return 1;
}

stock PlaySoundEx(soundid, Float:x, Float:y, Float:z, range)
{
	foreach(new i : Player)
	{
		if(!IsPlayerConnected(i)) continue;
		if(!IsPlayerInRangeOfPoint(i, range, x, y, z)) continue;
		PlayerPlaySound(i, soundid, 0, 0, 0);
 	}
}