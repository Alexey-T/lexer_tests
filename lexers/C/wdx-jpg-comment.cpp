/*
 * wdx-jpg-comment
 * TotalCommander content plugin for image metadata
 *
 * Copyright © 2006-2012 by Udo Liess, Thomas Beutlich
 * uses exiv2 (http://www.exiv2.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 * http://www.fsf.org/licensing/licenses/gpl.txt
 */

#include "stdafx.h"
#include "cunicode.h"
#include "contplug.h"
#include "resource.h"
#include "unsorted_map.hpp"
#include <math.h>
#include <string>
#include <sstream>
#include <vld.h>
#include "..\..\src\exiv2.hpp"
#include "..\..\src\image.hpp"
#include "..\..\xmpsdk\include\XMP_Const.h"

typedef struct
{
	void* FieldValue;
	int FieldIndex;
	int maxlen;
	int flags;
	char* langidentifier;
} WndVar;

typedef struct
{
	wchar_t fileName[wdirtypemax];
	FILETIME ftCreate;
	FILETIME ftAccess;
	FILETIME ftWrite;
	BOOL resetTimeStamp;
//	char iptcCharset[20];
} FileCacheVar;

typedef unsorted_map<std::string> multipleChoiceMap;

typedef struct
{
	char* fieldname;
	char* keyname;
	char* fieldunit;
	multipleChoiceMap* mcMap;
	int fieldtype;
	int fieldrationalbase;
	BOOL prettyprint;
} FieldVar;

#define _detectstring "EXT=\"JPG\"|EXT=\"JPEG\"|EXT=\"EXV\"|EXT=\"CR2\"|EXT=\"CRW\"|EXT=\"MRW\"|EXT=\"TIF\"|EXT=\"TIFF\"|EXT=\"DNG\"|EXT=\"NEF\"|EXT=\"PEF\"|EXT=\"ARW\"|EXT=\"RW2\"|EXT=\"SR2\"|EXT=\"SRW\"|EXT=\"ORF\"|EXT=\"PNG\"|EXT=\"PGF\"|EXT=\"RAF\"|EXT=\"XMP\"|EXT=\"PSD\"|EXT=\"JP2\"|EXT=\"EPS\""
#define fieldnames(i) fields[(i)].fieldname
#define fieldtypes(i) fields[(i)].fieldtype
#define fieldrationalbases(i) fields[(i)].fieldrationalbase
#define fieldprettyprint(i) fields[(i)].prettyprint
#define keynames(i) fields[(i)].keyname
#define fieldunits_and_multiplechoicestrings(i) fields[(i)].fieldunit
#define mcmap(i) fields[(i)].mcMap
#define fieldIsJPGComment(i) (0 == strnicmp(fields[(i)].keyname, "JPG", 3))
#define fieldIsExif(i) (0 == strnicmp(fields[(i)].keyname, "Exif", 4))
#define fieldIsIptc(i) (0 == strnicmp(fields[(i)].keyname, "Iptc", 4))
#define fieldIsXmp(i) (0 == strnicmp(fields[(i)].keyname, "Xmp", 3))
#define fieldIsSeparator(i) (0 == strcmp(fields[(i)].fieldname, "-"))
#define METADATA L"Metadata"
#define SETTINGS L"Settings"
#define PLUGIN_AUTHOR L"Thomas Beutlich"
#define PLUGIN L"ImageMetaData plugin 2.3.1.4"

// global variables
HINSTANCE hinst = nullptr;
CRITICAL_SECTION criti;
BOOL loaded = FALSE; // criti initialized
FileCacheVar fileCache;
volatile BOOL GetAborted;
static std::wstring lngFileName = L"";
static std::wstring iniFileName = L"";
Exiv2::Image::AutoPtr image(nullptr);
int fieldcount;
FieldVar* fields = nullptr;

INT_PTR CALLBACK DlgProc(HWND, UINT, WPARAM, LPARAM);

void logHandler(int lvl, const char* msg)
{
	UINT msgType = MB_OK;
	switch (lvl)
	{
		case Exiv2::LogMsg::debug:
		case Exiv2::LogMsg::info:
			msgType |= MB_ICONINFORMATION;
			break;

		case Exiv2::LogMsg::warn:
			msgType |= MB_ICONWARNING;
			break;

		case Exiv2::LogMsg::error:
			msgType |= MB_ICONERROR;
			break;

		default:
			return;
	}

	if (msgType != MB_ICONERROR || GetPrivateProfileIntT(SETTINGS, L"ShowErrors", 1, iniFileName.c_str()))
	{
		MessageBoxT(nullptr, Exiv2::s2ws(msg).c_str(), PLUGIN, msgType);
	}
}

char* strlcpy(char* p, const char* p2, int maxlen)
{
	if ((int)strlen(p2) >= maxlen - 1)
	{
		strncpy_s(p, maxlen, p2, maxlen - 1);
		p[maxlen] = 0;
	}
	else
	{
		strcpy_s(p, maxlen, p2);
	}
	return p;
}

std::string convertCRLF(std::string str)
{
	std::string::size_type startPos = 0;
	startPos = str.find("\r\n", startPos);
	// replace CR/LF by LF
	while (startPos != std::string::npos)
	{
		str.replace(startPos, 2, "\n");
		startPos = str.find("\r\n", startPos);
	}

	startPos = str.find("\r", 0);
	// replace CR by LF
	while (startPos != std::string::npos)
	{
		str.replace(startPos, 1, "\n");
		startPos = str.find("\r", startPos);
	}

	startPos = str.rfind("\n");
	// insert CR before LF
	while (startPos != std::string::npos)
	{
		str.insert(startPos, "\r");
		startPos = str.rfind("\n", startPos);
	}

	return str;
}

std::wstring convertCRLF(std::wstring str)
{
	std::wstring::size_type startPos = 0;
	startPos = str.find(L"\r\n", startPos);
	// replace CR/LF by LF
	while (startPos != std::wstring::npos)
	{
		str.replace(startPos, 2, L"\n");
		startPos = str.find(L"\r\n", startPos);
	}

	startPos = str.find(L"\r", 0);
	// replace CR by LF
	while (startPos != std::wstring::npos)
	{
		str.replace(startPos, 1, L"\n");
		startPos = str.find(L"\r", startPos);
	}

	startPos = str.rfind(L"\n");
	// insert CR before LF
	while (startPos != std::wstring::npos)
	{
		str.insert(startPos, L"\r");
		startPos = str.rfind(L"\n", startPos);
	}

	return str;
}

std::string& replaceAll(std::string& context, const std::string& from, const std::string& to)
{
    size_t lookHere = 0;
    size_t foundHere;
    while((foundHere = context.find(from, lookHere)) != std::string::npos)
    {
          context.replace(foundHere, from.size(), to);
          lookHere = foundHere + to.size();
    }
    return context;
}

std::string Token(std::string Src, std::string Delim, int number)
{
	for (std::string::size_type First, Last = 0; First = Src.find_first_not_of(Delim, Last), First != std::string::npos ? Last = min(Src.find_first_of(Delim, First), Src.size()), true : false;)
		if (!--number)
			return Src.substr(First, Last - First);
	return "";
}
/*
std::wstring Token(std::wstring Src, std::wstring Delim, int number)
{
	for (std::wstring::size_type First, Last = 0; First = Src.find_first_not_of(Delim, Last), First != std::wstring::npos ? Last = min(Src.find_first_of(Delim, First), Src.size()), true : false;)
		if (!--number)
			return Src.substr(First, Last - First);
	return L"";
}
*/
BOOL APIENTRY DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved)
{
	switch (fdwReason)
	{
		case DLL_PROCESS_ATTACH :
		{
			hinst = hinstDLL;
			DisableThreadLibraryCalls(hinst);
			break;
		}

		case DLL_PROCESS_DETACH :
			hinst = nullptr;
			break;
	}
	return TRUE;
}

INT_PTR CALLBACK DlgProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg)
	{
		case WM_INITDIALOG:
		{
			WndVar* hWndVar = (WndVar*) lParam;
#ifndef _WIN64
			SetWindowLongPtr(hWnd, GWL_USERDATA, (LONG)lParam);
#else
			SetWindowLongPtr(hWnd, GWLP_USERDATA, (LONG)lParam);
#endif
			if (hWndVar && hWndVar->FieldValue && hWndVar->flags == editflags_initialize)
			{
				switch (fieldtypes(hWndVar->FieldIndex))
				{
					case ft_string:
						SetDlgItemText(hWnd, IDC_EDIT, (const char*)hWndVar->FieldValue);
						break;

					case ft_stringw:
						SetDlgItemTextT(hWnd, IDC_EDIT, (const wchar_t*)hWndVar->FieldValue);
						break;
				}
			}
			if (hWndVar && hWndVar->langidentifier)
			{
				wchar_t captionW[_MAX_PATH];
				wchar_t appNameW[_MAX_PATH];
				wchar_t keyNameW[_MAX_PATH];
				wchar_t defaultNameW[_MAX_PATH];
				awlcopy(appNameW, hWndVar->langidentifier, countof(appNameW) - 1);
				awlcopy(keyNameW, fieldnames(hWndVar->FieldIndex), countof(keyNameW) - 1);
				awlcopy(defaultNameW, fieldnames(hWndVar->FieldIndex), countof(defaultNameW) - 1);
				GetPrivateProfileStringT(appNameW, keyNameW, defaultNameW, captionW, _MAX_PATH - 1, lngFileName.c_str());
				char caption[_MAX_PATH];
				walcopy(caption, captionW, countof(caption) - 1);
				SetWindowText(hWnd, caption);
			}
			else
			{
				SetWindowText(hWnd, fieldnames(hWndVar->FieldIndex));
			}

			{
				// center dialog with respect to parent window
				RECT parentArea;
				RECT dlgArea;

				ZeroMemory(&parentArea, sizeof(RECT));
				ZeroMemory(&dlgArea, sizeof(RECT));

				GetWindowRect(GetParent(hWnd), &parentArea);
				GetWindowRect(hWnd, &dlgArea);
				SetWindowPos(hWnd, 0, (parentArea.right + parentArea.left - dlgArea.right + dlgArea.left)/2, (parentArea.bottom + parentArea.top - dlgArea.bottom + dlgArea.top)/2, dlgArea.right - dlgArea.left, dlgArea.bottom - dlgArea.top, SWP_SHOWWINDOW);
			}
			break;
		}

		case WM_CLOSE:
			EndDialog(hWnd, IDCANCEL);
			break;

		case WM_COMMAND:
			if (wParam == IDOK)
			{
#ifndef _WIN64
				WndVar * hWndVar = (WndVar*)(LONG_PTR)GetWindowLongPtr(hWnd, GWL_USERDATA);
#else
				WndVar * hWndVar = (WndVar*)(LONG_PTR)GetWindowLongPtr(hWnd, GWLP_USERDATA);
#endif
				if (hWndVar && hWndVar->FieldValue)
				{
					switch (fieldtypes(hWndVar->FieldIndex))
					{
						case ft_string:
							GetDlgItemText(hWnd, IDC_EDIT, (char*)hWndVar->FieldValue, hWndVar->maxlen);
							break;

						case ft_stringw:
							GetDlgItemTextT(hWnd, IDC_EDIT, (wchar_t*)hWndVar->FieldValue, hWndVar->maxlen/2 - 1);
							break;
					}
				}
				EndDialog(hWnd, IDOK);
			}
			else if (wParam == IDCANCEL)
			{
				EndDialog(hWnd, IDCANCEL);
			}
			break;

		default:
			return FALSE;
	}

	return TRUE;
}

int __stdcall ContentGetSupportedField(int FieldIndex, char* FieldName, char* Units, int maxlen)
{
	if (fields == nullptr)
	{
		if (!loaded)
		{
			InitializeCriticalSection(&criti);
			EnterCriticalSection(&criti);
			loaded = TRUE;
			LeaveCriticalSection(&criti);
		}

		EnterCriticalSection(&criti);
		SecureZeroMemory(&fileCache, sizeof(FileCacheVar));
		fieldcount = 0;
		LeaveCriticalSection(&criti);
		wchar_t tagsW[20*_MAX_PATH];
		wchar_t *ptagsW = &tagsW[0];
		GetPrivateProfileStringT(METADATA, nullptr, L"", tagsW, 20*_MAX_PATH - 1, iniFileName.c_str());
		EnterCriticalSection(&criti);
		while (*ptagsW)
		{
			fieldcount++;
			while(*ptagsW++);
		}
		LeaveCriticalSection(&criti);
		if (fieldcount > 0)
		{
			EnterCriticalSection(&criti);
			fields = (FieldVar*) malloc(fieldcount*sizeof(FieldVar));
			if (fields)
			{
				SecureZeroMemory(fields, fieldcount*sizeof(FieldVar));
				ptagsW = &tagsW[0];
				char fieldLine[20*_MAX_PATH];
				wchar_t fieldLineW[20*_MAX_PATH];
				int i = 0;
				while (*ptagsW)
				{
					GetPrivateProfileStringT(METADATA, ptagsW, L"", fieldLineW, 20*_MAX_PATH - 1, iniFileName.c_str());
					walcopy(fieldLine, fieldLineW, countof(fieldLine) - 1);
					size_t lineLength = strlen(fieldLine);
					char* token = strtok(fieldLine, "|");
					if (token != nullptr)
					{
						fieldtypes(i) = atoi(token);
						if ((fieldtypes(i) == ft_string) && (0 == strnicmp(token, "8PP", 3)))
						{
							fieldprettyprint(i) = TRUE;
						}
						fieldrationalbases(i) = atoi(Token(token, "R", 2).c_str());
						token = strtok(nullptr, "|");
						if (token != nullptr)
						{
							fieldnames(i) = (char*)malloc((strlen(token) + 1)*sizeof(char));
							if (fieldnames(i))
							{
								strcpy(fieldnames(i), token);
								token = strtok(nullptr, "|");
								if (token != nullptr)
								{
									keynames(i) = (char*)malloc((strlen(token) + 1)*sizeof(char));
									if (keynames(i))
									{
										strcpy(keynames(i), token);
										size_t tokenLength = token + strlen(token) + 1 - fieldLine;
										if (((fieldtypes(i) == ft_multiplechoice) || fieldIsJPGComment(i)) && (tokenLength <= lineLength))
										{
											token = &fieldLine[tokenLength];
											char* mcToken = strtok(token, "|");
											mcmap(i) = new multipleChoiceMap;
											while(mcToken != nullptr)
											{
												std::string to1 = Token(mcToken, "=", 1);
												std::string to2 = Token(mcToken, "=", 2);
												if (to2.empty())
												{
													to2 = to1;
												}
												mcmap(i)->push_back(to1, to2);
												mcToken = strtok(nullptr, "|");
											}
											multipleChoiceMap::iterator iter;
											size_t mcLength = mcmap(i)->size();
											for (iter = mcmap(i)->begin(); iter != mcmap(i)->end(); iter++)
											{
												mcLength += ((*iter).second).size();
											}

											fieldunits_and_multiplechoicestrings(i) = (char*)malloc((mcLength + 1)*sizeof(char));
											if (fieldunits_and_multiplechoicestrings(i))
											{
												iter = mcmap(i)->begin();
												strcpy(fieldunits_and_multiplechoicestrings(i), ((*iter++).second).c_str());
												strcat(fieldunits_and_multiplechoicestrings(i), "|");
												for (iter; iter != mcmap(i)->end(); iter++)
												{
													strcat(fieldunits_and_multiplechoicestrings(i), ((*iter).second).c_str());
													strcat(fieldunits_and_multiplechoicestrings(i), "|");
												}
												fieldunits_and_multiplechoicestrings(i)[mcLength - 1] = '\0';
											}
											else
											{
												LeaveCriticalSection(&criti);
												return ft_nomorefields;
											}
										}
										else
										{
											fieldunits_and_multiplechoicestrings(i) = (char*)malloc(sizeof(char));
											if (fieldunits_and_multiplechoicestrings(i))
											{
												fieldunits_and_multiplechoicestrings(i)[0] = '\0';
											}
											else
											{
												LeaveCriticalSection(&criti);
												return ft_nomorefields;
											}
										}
									}
									else
									{
										LeaveCriticalSection(&criti);
										return ft_nomorefields;
									}
								}
							}
							else
							{
								LeaveCriticalSection(&criti);
								return ft_nomorefields;
							}
						}
					}
					i++;
					while(*ptagsW++);
				}
			}
			else
			{
				LeaveCriticalSection(&criti);
				return ft_nomorefields;
			}
			LeaveCriticalSection(&criti);
		}
		else
		{
			return ft_nomorefields;
		}
	}

	if (FieldIndex < 0 || FieldIndex >= fieldcount)
	{
		return ft_nomorefields;
	}
	strlcpy(FieldName, fieldnames(FieldIndex), maxlen - 1);
	strlcpy(Units, fieldunits_and_multiplechoicestrings(FieldIndex), maxlen - 1);
	return fieldtypes(FieldIndex);
}

int __stdcall ContentGetSupportedFieldFlags(int FieldIndex)
{
	switch (FieldIndex)
	{
		case -1:
			return contflags_edit | contflags_fieldedit;

		default:
			switch (fieldtypes(FieldIndex))
			{
				case ft_date:
				case ft_datetime:
				case ft_time:
				case ft_numeric_32:
				case ft_numeric_64:
				case ft_numeric_floating:
					return contflags_edit;

				default:
					if (fieldprettyprint(FieldIndex))
					{
						return 0;
					}
					else if ((!fieldIsSeparator(FieldIndex)) && (0 == strcmp("", fieldunits_and_multiplechoicestrings(FieldIndex)) || fieldIsExif(FieldIndex) || fieldIsXmp(FieldIndex) || fieldIsIptc(FieldIndex)))
					{
						return contflags_edit | contflags_fieldedit;
					}
					else
					{
						return 0;
					}
			}
	}
}

int __stdcall ContentGetDetectString(char* DetectString, int maxlen)
{
	strlcpy(DetectString, _detectstring, maxlen);
	return 0;
}

int __stdcall ContentGetValue(char* FileName, int FieldIndex, int UnitIndex, void* FieldValue, int maxlen, int flags)
{
	wchar_t FileNameW[wdirtypemax];
	int retVal = ContentGetValueW(awfilenamecopy(FileNameW, FileName), FieldIndex, UnitIndex, FieldValue, maxlen, flags);
	if (retVal == ft_numeric_floating)
	{
		wchar_t* p = wcsdup((const wchar_t*)((char*)FieldValue + sizeof(double)));
		walcopy((char*)FieldValue + sizeof(double), p, maxlen - sizeof(double) - 1);
		free(p);
	}
	return retVal;
}

int __stdcall ContentGetValueW(wchar_t* FileName, int FieldIndex, int UnitIndex, void* FieldValue, int maxlen, int flags)
{
	if (flags & CONTENT_DELAYIFSLOW)
	{
		return ft_delayed;
	}

	if (fieldIsSeparator(FieldIndex))
	{
		return ft_nosuchfield;
	}
	else if (wcscmp(fileCache.fileName, FileName))
	{
		if (!loaded)
		{
			InitializeCriticalSection(&criti);
			EnterCriticalSection(&criti);
			loaded = TRUE;
			LeaveCriticalSection(&criti);
		}

		EnterCriticalSection(&criti);
		GetAborted = FALSE;
		wcscpy(fileCache.fileName, FileName);
		LeaveCriticalSection(&criti);
		try
		{
			EnterCriticalSection(&criti);
			image = Exiv2::ImageFactory::open(FileName);
			if (image->good())
			{
				image->readMetadata();
			}
			else
			{
				if (GetPrivateProfileIntT(SETTINGS, L"ShowErrors", 1, iniFileName.c_str()))
				{
					MessageBoxT(nullptr, L"Error: Image is not good", FileName, MB_ICONSTOP | MB_OK);
				}
				LeaveCriticalSection(&criti);
				return ft_fileerror;
			}
		}
		catch (const Exiv2::WError& e)
		{
			if (GetPrivateProfileIntT(SETTINGS, L"ShowErrors", 1, iniFileName.c_str()))
			{
				MessageBoxT(nullptr, e.wwhat(), FileName, MB_ICONSTOP | MB_OK);
			}
			LeaveCriticalSection(&criti);
			return ft_fileerror;
		}
		catch (const std::exception& e)
		{
			if (GetPrivateProfileIntT(SETTINGS, L"ShowErrors", 1, iniFileName.c_str()))
			{
				MessageBoxT(nullptr, Exiv2::s2ws(e.what()).c_str(), FileName, MB_ICONSTOP | MB_OK);
			}
			LeaveCriticalSection(&criti);
			return ft_fileerror;
		}
		catch (...)
		{
			LeaveCriticalSection(&criti);
			return ft_fileerror;
		}
//		strlcpy(fileCache.iptcCharset, image->iptcData().detectCharset(), 19);
		LeaveCriticalSection(&criti);
	}
	else if (image.get() == nullptr)
	{
		return ft_fileerror;
	}

	if (FieldIndex < fieldcount)
	{
		if (GetAborted)
		{
			return ft_fieldempty;
		}
		try
		{
			if (fieldIsJPGComment(FieldIndex))
			{
				std::string comment = image->comment();
				switch (fieldtypes(FieldIndex))
				{
					case ft_string:
						if (0 == strcmp("", fieldunits_and_multiplechoicestrings(FieldIndex))) // JPG Comment Complete
						{
							strlcpy((char*)FieldValue, comment.c_str(), maxlen - 1);
						}
						else // JPG Comment Line
						{
							strlcpy((char*)FieldValue, Token(comment, "\r\n", UnitIndex + 1).c_str(), maxlen - 1);
						}
						break;

					case ft_stringw:
						if (0 == strcmp("", fieldunits_and_multiplechoicestrings(FieldIndex))) // JPG Comment Complete
						{
							wcsncpy((wchar_t*)FieldValue, Exiv2::s2ws(comment).c_str(), maxlen/2 - 1);
						}
						else // JPG Comment Line
						{
							wcsncpy((wchar_t*)FieldValue, Exiv2::s2ws(Token(comment, "\r\n", UnitIndex + 1)).c_str(), maxlen/2 - 1);
						}
						break;

					default:
					{
						char msg[_MAX_PATH];
						wsprintfA(msg, "Error: Field type %d is not supported for \"%s\"", fieldtypes(FieldIndex), keynames(FieldIndex));
						if (GetPrivateProfileIntT(SETTINGS, L"ShowErrors", 1, iniFileName.c_str()))
						{
							MessageBoxT(nullptr, Exiv2::s2ws(msg).c_str(), FileName, MB_ICONSTOP | MB_OK);
						}
						return ft_fileerror;
					}
				}
			}
			else if (fieldIsExif(FieldIndex))
			{
				Exiv2::ExifData &exifData = image->exifData();
				Exiv2::ExifKey exifKey(keynames(FieldIndex));
				Exiv2::ExifData::const_iterator iter = exifData.findKey(exifKey);
				if (iter != exifData.end())
				{
					switch (fieldtypes(FieldIndex))
					{
						case ft_numeric_32:
							*((int*)FieldValue) = (int) iter->value().toLong();
							break;

						case ft_numeric_64:
							*((long long*)FieldValue) = (long long) iter->value().toLong();
							break;

						case ft_numeric_floating:
						{
							if (fieldrationalbases(FieldIndex) == 0)
							{
								*((double*)FieldValue) = (double) iter->value().toFloat();
							}
							else
							{
								*(double*)FieldValue = 0.0;
								int n = iter->value().count();
								for (int i = 0; i < n; i++)
								{
									const int32_t d = iter->value().toRational(i).second;
									if (d == 0)
									{
										return ft_fieldempty;
									}
									const int32_t z = iter->value().toRational(i).first;
									*(double*)FieldValue += static_cast<double>(z)/d/pow(static_cast<double>(fieldrationalbases(FieldIndex)), i);
								}
							}
							std::wstringstream wss;
							wss << std::setiosflags(std::ios::fixed) << std::setprecision(8) << *(double*)FieldValue;
							const std::wstring ws(wss.str());
							wcsncpy_s((wchar_t*)((char*)FieldValue + sizeof(double)), (maxlen - 9)/2, ws.c_str(), _TRUNCATE);
							break;
						}

						case ft_multiplechoice:
						{
							multipleChoiceMap::const_iterator choice = mcmap(FieldIndex)->find(iter->value().toString());
							if (choice != mcmap(FieldIndex)->end())
							{
								strlcpy((char*)FieldValue, ((*choice).second).c_str(), maxlen - 1);
							}
							else
							{
								return ft_fileerror;
							}
							break;
						}

						case ft_string:
							if (fieldprettyprint(FieldIndex))
							{
								strlcpy((char*)FieldValue, iter->print(&exifData).c_str(), maxlen - 1);
							}
							else if (0 == iter->value().toString().compare(0, 17, "charset=\"Unicode\" ", 0, 17))
							{
								Exiv2::DataBuf buf(iter->value().size());
								iter->value().copy(buf.pData_, Exiv2::bigEndian);
								std::string str(reinterpret_cast<char*>(buf.pData_) + 8, buf.size_ - 8);
								std::string::size_type pos = str.find_last_not_of('\0');
								WideCharToMultiByte(CP_ACP, 0, LPCWSTR(str.substr(0, pos + 1).c_str()), (pos + 2)/2, (char*)FieldValue, maxlen, nullptr, nullptr);
							}
							else if (Exiv2::unsignedByte == iter->typeId())
							{
								Exiv2::DataBuf buf(iter->value().size());
								iter->value().copy(buf.pData_, Exiv2::bigEndian);
								std::string str(reinterpret_cast<char*>(buf.pData_), buf.size_);
								std::string::size_type pos = str.find_last_not_of('\0');
								WideCharToMultiByte(CP_ACP, 0, LPCWSTR(str.c_str()), (pos + 4)/2, (char*)FieldValue, maxlen, nullptr, nullptr);
							}
							else
							{
								strlcpy((char*)FieldValue, iter->value().toString().c_str(), maxlen - 1);
							}
							break;

						case ft_stringw:
						{
							std::string s = iter->value().toString();
							int slength = (int)s.length() + 1;
							int len = MultiByteToWideChar(CP_UTF8, 0, s.c_str(), slength, 0, 0);
							wchar_t* buf = new wchar_t[len];
							MultiByteToWideChar(CP_UTF8, 0, s.c_str(), slength, buf, len);
							std::wstring r(buf);
							delete[] buf;
							wcsncpy((wchar_t*)FieldValue, convertCRLF(r).c_str(), maxlen/2 - 1);
							break;
						}

						case ft_date:
						{
							unsigned int Year = 0 , Month = 0, Day = 0;
							if (3 == sscanf(iter->value().toString().c_str(), "%04u:%02u:%02u", &Year, &Month, &Day) ||
								3 == sscanf(iter->print(&exifData).c_str(), "%04u:%02u:%02u", &Year, &Month, &Day))
							{
								((pdateformat)FieldValue)->wYear = (WORD)Year;
								((pdateformat)FieldValue)->wMonth = (WORD)Month;
								((pdateformat)FieldValue)->wDay = (WORD)Day;
							}
							else
							{
								SecureZeroMemory(FieldValue, sizeof(tdateformat));
							}
							break;
						}

						case ft_time:
						{
							unsigned int Hour = 0 , Minute = 0, Second = 0;
							if (3 == sscanf(iter->print(&exifData).c_str(), "%02u:%02u:%02u", &Hour, &Minute, &Second))
							{
								((ptimeformat)FieldValue)->wHour = (WORD)Hour;
								((ptimeformat)FieldValue)->wMinute = (WORD)Minute;
								((ptimeformat)FieldValue)->wSecond = (WORD)Second;
							}
							else
							{
								SecureZeroMemory(FieldValue, sizeof(ttimeformat));
							}
							break;
						}

						case ft_datetime:
						{
							SYSTEMTIME systime = {0};
							if (6 == sscanf(iter->value().toString().c_str(), "%4u:%02u:%02u %02u:%02u:%02u", &systime.wYear, &systime.wMonth, &systime.wDay, &systime.wHour, &systime.wMinute, &systime.wSecond) ||
								3 == sscanf(iter->value().toString().c_str(), "%4u:%02u:%02u", &systime.wYear, &systime.wMonth, &systime.wDay))
							{
								// time zone correction
								FILETIME ft = {0};
								SystemTimeToFileTime(&systime, &ft);
								LocalFileTimeToFileTime(&ft, (FILETIME*) FieldValue);
							}
							break;
						}

						default:
						{
							char msg[_MAX_PATH];
							wsprintfA(msg, "Error: Field type %d is not supported for EXIF tag \"%s\"", fieldtypes(FieldIndex), keynames(FieldIndex));
							if (GetPrivateProfileIntT(SETTINGS, L"ShowErrors", 1, iniFileName.c_str()))
							{
								MessageBoxT(nullptr, Exiv2::s2ws(msg).c_str(), FileName, MB_ICONSTOP | MB_OK);
							}
							return ft_fileerror;
						}
					}
				}
				else
				{
					return ft_fieldempty;
				}
			}
			else if (fieldIsIptc(FieldIndex))
			{
				Exiv2::IptcData &iptcData = image->iptcData();
				Exiv2::IptcKey iptcKey(keynames(FieldIndex));
				Exiv2::IptcData::iterator iter = iptcData.findKey(iptcKey);
				if (iter == iptcData.end())
				{
					return ft_fieldempty;
				}
				switch (fieldtypes(FieldIndex))
				{
					case ft_numeric_32:
						*((int*)FieldValue) = (int) iter->value().toLong();
						break;

					case ft_numeric_64:
						*((long long*)FieldValue) = (long long) iter->value().toLong();
						break;

					case ft_numeric_floating:
					{
						*((double*)FieldValue) = (double) iter->value().toFloat();
						std::wstringstream wss;
						wss << std::setiosflags(std::ios::fixed) << std::setprecision(8) << *(double*)FieldValue;
						const std::wstring ws(wss.str());
						wcsncpy_s((wchar_t*)((char*)FieldValue + sizeof(double)), (maxlen - 9)/2, ws.c_str(), _TRUNCATE);
						break;
					}

					case ft_multiplechoice:
					{
						multipleChoiceMap::const_iterator choice = mcmap(FieldIndex)->find(iter->value().toString());
						if (choice != mcmap(FieldIndex)->end())
						{
							strlcpy((char*)FieldValue, ((*choice).second).c_str(), maxlen - 1);
						}
						else
						{
							return ft_fileerror;
						}
						break;
					}

					case ft_string:
						if (Exiv2::IptcDataSets::dataSetRepeatable(iptcKey.tag(), iptcKey.record()))
						{
							BOOL first = TRUE;
							for (iter = iter; iter != iptcData.end(); ++iter)
							{
								if (first)
								{
									first = FALSE;
									strlcpy((char*)FieldValue, iter->value().toString().c_str(), maxlen - 1);
								}
								else
								{
									if (iter->key() != std::string(keynames(FieldIndex)))
									{
										continue;
									}

									strcat((char*)FieldValue, "\r\n");
									strcat((char*)FieldValue, iter->value().toString().c_str());
								}
							}
						}
						else
						{
							strlcpy((char*)FieldValue, convertCRLF(iter->value().toString()).c_str(), maxlen - 1);
						}
						break;

					case ft_stringw:
						if (Exiv2::IptcDataSets::dataSetRepeatable(iptcKey.tag(), iptcKey.record()))
						{
							BOOL first = TRUE;
							for (iter = iter; iter != iptcData.end(); ++iter)
							{
								if (first)
								{
									first = FALSE;
									std::string s = iter->value().toString();
									int slength = (int)s.length() + 1;
									int len = MultiByteToWideChar(CP_UTF8, 0, s.c_str(), slength, 0, 0);
									wchar_t* buf = new wchar_t[len];
									MultiByteToWideChar(CP_UTF8, 0, s.c_str(), slength, buf, len);
									std::wstring r(buf);
									delete[] buf;
									wcsncpy((wchar_t*)FieldValue, convertCRLF(r).c_str(), maxlen/2 - 1);
								}
								else
								{
									if (iter->key() != std::string(keynames(FieldIndex)))
									{
										continue;
									}

									std::string s = iter->value().toString();
									int slength = (int)s.length() + 1;
									int len = MultiByteToWideChar(CP_UTF8, 0, s.c_str(), slength, 0, 0);
									wchar_t* buf = new wchar_t[len];
									MultiByteToWideChar(CP_UTF8, 0, s.c_str(), slength, buf, len);
									std::wstring r(buf);
									delete[] buf;
									wcslcat((wchar_t*)FieldValue, L"\r\n", maxlen/2 - 1);
									wcslcat((wchar_t*)FieldValue, convertCRLF(r).c_str(), maxlen/2 - 1);
								}
							}
						}
						else
						{
							std::string s = iter->value().toString();
							int slength = (int)s.length() + 1;
							int len = MultiByteToWideChar(CP_UTF8, 0, s.c_str(), slength, 0, 0);
							wchar_t* buf = new wchar_t[len];
							MultiByteToWideChar(CP_UTF8, 0, s.c_str(), slength, buf, len);
							std::wstring r(buf);
							delete[] buf;
							wcsncpy((wchar_t*)FieldValue, convertCRLF(r).c_str(), maxlen/2 - 1);
						}
						break;

					case ft_date:
					{
						int Year = 0 , Month = 0, Day = 0;
						if (3 == sscanf(iter->value().toString().c_str(), "%u-%u-%u", &Year, &Month, &Day))
						{
							((pdateformat)FieldValue)->wYear = Year;
							((pdateformat)FieldValue)->wMonth = Month;
							((pdateformat)FieldValue)->wDay = Day;
						}
						else
						{
							SecureZeroMemory(FieldValue, sizeof(tdateformat));
						}
						break;
					}

					case ft_time:
					{
						int Hour = 0 , Minute = 0, Second = 0, HourOffset = 0 , MinOffset = 0;
						char Offset = '+';
						sscanf(iter->value().toString().c_str(), "%u:%u:%u%c%u:%u", &Hour, &Minute, &Second, &Offset, &HourOffset, &MinOffset);
						((ptimeformat)FieldValue)->wHour = Hour;
						((ptimeformat)FieldValue)->wMinute = Minute;
						((ptimeformat)FieldValue)->wSecond = Second;
						break;
					}

					default:
					{
						char msg[_MAX_PATH];
						wsprintfA(msg, "Error: Field type %d is not supported for IPTC tag \"%s\"", fieldtypes(FieldIndex), keynames(FieldIndex));
						if (GetPrivateProfileIntT(SETTINGS, L"ShowErrors", 1, iniFileName.c_str()))
						{
							MessageBoxT(nullptr, Exiv2::s2ws(msg).c_str(), FileName, MB_ICONSTOP | MB_OK);
						}
						return ft_fileerror;
					}
				}
			}
			else if (fieldIsXmp(FieldIndex))
			{
				Exiv2::XmpData& xmpData = image->xmpData();
				Exiv2::XmpKey xmpKey(keynames(FieldIndex));
				Exiv2::XmpData::iterator iter = xmpData.findKey(xmpKey);
				if (iter == xmpData.end())
				{
					return ft_fieldempty;
				}
				switch (fieldtypes(FieldIndex))
				{
					case ft_numeric_32:
						*((int*)FieldValue) = (int) iter->value().toLong();
						break;

					case ft_numeric_64:
						*((long long*)FieldValue) = (long long) iter->value().toLong();
						break;

					case ft_numeric_floating:
					{
						*((double*)FieldValue) = (double) iter->value().toFloat();
						std::wstringstream wss;
						wss << std::setiosflags(std::ios::fixed) << std::setprecision(8) << *(double*)FieldValue;
						const std::wstring ws(wss.str());
						wcsncpy_s((wchar_t*)((char*)FieldValue + sizeof(double)), (maxlen - 9)/2, ws.c_str(), _TRUNCATE);
						break;
					}

					case ft_multiplechoice:
					{
						multipleChoiceMap::const_iterator choice = mcmap(FieldIndex)->find(iter->value().toString());
						if (choice != mcmap(FieldIndex)->end())
						{
							strlcpy((char*)FieldValue, ((*choice).second).c_str(), maxlen - 1);
						}
						else
						{
							return ft_fileerror;
						}
						break;
					}

					case ft_string:
						strlcpy((char*)FieldValue, iter->value().toString().c_str(), maxlen - 1);
						break;

					case ft_stringw:
					{
						std::string s = iter->value().toString();
						int slength = (int)s.length() + 1;
						int len = MultiByteToWideChar(CP_UTF8, 0, s.c_str(), slength, 0, 0);
						wchar_t* buf = new wchar_t[len];
						MultiByteToWideChar(CP_UTF8, 0, s.c_str(), slength, buf, len);
						std::wstring r(buf);
						delete[] buf;
						wcsncpy((wchar_t*)FieldValue, convertCRLF(r).c_str(), maxlen/2 - 1);
						break;
					}

					case ft_date:
					{
						int Year = 0 , Month = 0, Day = 0;
						if (3 == sscanf(iter->value().toString().c_str(), "%4u-%02u-%02u", &Year, &Month, &Day) ||
							2 == sscanf(iter->value().toString().c_str(), "%4u-%02u", &Year, &Month) ||
							1 == sscanf(iter->value().toString().c_str(), "%4u", &Year))
						{
							((pdateformat)FieldValue)->wYear = Year;
							((pdateformat)FieldValue)->wMonth = Month;
							((pdateformat)FieldValue)->wDay = Day;
						}
						break;
					}

					case ft_time:
					{
						int Year = 0, Month = 0, Day = 0, Hour = 0 , Minute = 0, Second = 0;
						if (6 == sscanf(iter->value().toString().c_str(), "%4u-%02u-%02uT%02u:%02u:%02u", &Year, &Month, &Day, &Hour, &Minute, &Second) ||
							5 == sscanf(iter->value().toString().c_str(), "%4u-%02u-%02uT%02u:%02u", &Year, &Month, &Day, &Hour, &Minute))
						{
							((ptimeformat)FieldValue)->wHour = Hour;
							((ptimeformat)FieldValue)->wMinute = Minute;
							((ptimeformat)FieldValue)->wSecond = Second;
						}
						break;
					}

					case ft_datetime:
					{
						SYSTEMTIME systime = {0};
						char TZD = '\0';
						unsigned int SecondFrac = 0;
						if (8 == sscanf(iter->value().toString().c_str(), "%4u-%02u-%02uT%02u:%02u:%02u.%u%c", &systime.wYear, &systime.wMonth, &systime.wDay, &systime.wHour, &systime.wMinute, &systime.wSecond, &SecondFrac, &TZD) ||
							7 == sscanf(iter->value().toString().c_str(), "%4u-%02u-%02uT%02u:%02u:%02u%c", &systime.wYear, &systime.wMonth, &systime.wDay, &systime.wHour, &systime.wMinute, &systime.wSecond, &TZD) ||
							6 == sscanf(iter->value().toString().c_str(), "%4u-%02u-%02uT%02u:%02u%c", &systime.wYear, &systime.wMonth, &systime.wDay, &systime.wHour, &systime.wMinute, &TZD) ||
							6 == sscanf(iter->value().toString().c_str(), "%4u-%02u-%02uT%02u:%02u:%02u", &systime.wYear, &systime.wMonth, &systime.wDay, &systime.wHour, &systime.wMinute, &systime.wSecond) ||
							5 == sscanf(iter->value().toString().c_str(), "%4u-%02u-%02uT%02u:%02u", &systime.wYear, &systime.wMonth, &systime.wDay, &systime.wHour, &systime.wMinute) ||
							3 == sscanf(iter->value().toString().c_str(), "%4u-%02u-%02u", &systime.wYear, &systime.wMonth, &systime.wDay) ||
							2 == sscanf(iter->value().toString().c_str(), "%4u-%02u", &systime.wYear, &systime.wMonth) ||
							1 == sscanf(iter->value().toString().c_str(), "%4u", &systime.wYear))
						{
							if (TZD == 'Z')
							{
								SystemTimeToFileTime(&systime, (FILETIME*) FieldValue);
							}
							else
							{
								// time zone correction
								FILETIME ft = {0};
								SystemTimeToFileTime(&systime, &ft);
								LocalFileTimeToFileTime(&ft, (FILETIME*) FieldValue);
							}
						}
						break;
					}

					default:
					{
						char msg[_MAX_PATH];
						wsprintfA(msg, "Error: Field type %d is not supported for XMP tag \"%s\"", fieldtypes(FieldIndex), keynames(FieldIndex));
						if (GetPrivateProfileIntT(SETTINGS, L"ShowErrors", 1, iniFileName.c_str()))
						{
							MessageBoxT(nullptr, Exiv2::s2ws(msg).c_str(), FileName, MB_ICONSTOP | MB_OK);
						}
						return ft_fileerror;
					}
				}
			}
			else
			{
				if (GetPrivateProfileIntT(SETTINGS, L"ShowErrors", 1, iniFileName.c_str()))
				{
					MessageBoxT(nullptr, L"Error: Invalid field", FileName, MB_ICONSTOP | MB_OK);
				}
				return ft_fileerror;
			}
		}
		catch (const XMP_Error& e)
		{
			if (GetPrivateProfileIntT(SETTINGS, L"ShowErrors", 1, iniFileName.c_str()))
			{
				MessageBoxT(nullptr, Exiv2::s2ws(e.GetErrMsg()).c_str(), FileName, MB_ICONSTOP | MB_OK);
			}
			return ft_fileerror;
		}
		catch (const Exiv2::WError& e)
		{
			if (GetPrivateProfileIntT(SETTINGS, L"ShowErrors", 1, iniFileName.c_str()))
			{
				MessageBoxT(nullptr, e.wwhat(), FileName, MB_ICONSTOP | MB_OK);
			}
			return ft_fileerror;
		}
		catch (const std::exception& e)
		{
			if (GetPrivateProfileIntT(SETTINGS, L"ShowErrors", 1, iniFileName.c_str()))
			{
				MessageBoxT(nullptr, Exiv2::s2ws(e.what()).c_str(), FileName, MB_ICONSTOP | MB_OK);
			}
			return ft_fileerror;
		}
		catch (...)
		{
			return ft_fileerror;
		}
	}
	else
	{
		return ft_nosuchfield;
	}

	return fieldtypes(FieldIndex); // very important!
}

int __stdcall ContentEditValue(HWND ParentWin, int FieldIndex, int UnitIndex, int FieldType, void* FieldValue, int maxlen, int flags, char* langidentifier)
{
	int retVal = ft_nosuchfield;

	if (FieldValue && FieldIndex >= 0 && FieldIndex < fieldcount && !fieldIsSeparator(FieldIndex))
	{
		WndVar * hWndVar = (WndVar *) malloc(sizeof(WndVar));
		if (hWndVar)
		{
			hWndVar->FieldValue = FieldValue;
			hWndVar->FieldIndex = FieldIndex;
			hWndVar->maxlen = maxlen;
			hWndVar->flags = flags;
			hWndVar->langidentifier = langidentifier;
			INT_PTR result = DialogBoxParam(hinst, MAKEINTRESOURCE(IDD_EDIT), ParentWin, DlgProc, (LPARAM) hWndVar);
			free(hWndVar);
			retVal = result == IDOK ? ft_setsuccess : ft_setcancel;
		}
	}

	return retVal;
}

void __stdcall ContentPluginUnloading(void)
{
	if (fields)
	{
		for (int i = 0; i < fieldcount; i++)
		{
			free(fieldnames(i));
			free(keynames(i));
			free(fieldunits_and_multiplechoicestrings(i));
			if (mcmap(i))
			{
				mcmap(i)->clear();
				delete mcmap(i);
			}
		}
		free(fields);
		fields = nullptr;
		fieldcount = 0;
	}

	if (loaded)
	{
		EnterCriticalSection(&criti);
		loaded = FALSE;
		LeaveCriticalSection(&criti);
		DeleteCriticalSection(&criti);
	}
}

void __stdcall ContentSetDefaultParams(ContentDefaultParamStruct* dps)
{
	Exiv2::LogMsg::setHandler(logHandler);

	wchar_t Path[wdirtypemax];
	std::wstring lngFileNameLookFirst = L"";
	std::wstring iniFileNameLookFirst = L"";
	if (GetModuleFileNameT(hinst, Path, wdirtypemax - 1) != 0)
	{
		char dd[_MAX_DRIVE];
		char pd[_MAX_PATH];
		wchar_t wdp[_MAX_DRIVE];
		wchar_t wpp[_MAX_PATH];
		wchar_t wfp[_MAX_FNAME];
		_splitpath_s(dps->DefaultIniName, dd, _MAX_DRIVE, pd, _MAX_PATH, nullptr, 0, nullptr, 0);
		_wsplitpath_s(Path, wdp, _MAX_DRIVE, wpp, _MAX_PATH, wfp, _MAX_FNAME, nullptr, 0);
		wchar_t wdd[_MAX_DRIVE];
		wchar_t wpd[_MAX_PATH];
		awlcopy(wdd, dd, _MAX_DRIVE - 1);
		awlcopy(wpd, pd, _MAX_PATH - 1);
		lngFileName = std::wstring(wdd) + std::wstring(wpd) + wfp + L".lng";
		lngFileNameLookFirst = std::wstring(wdp) + std::wstring(wpp) + wfp + L".lng";
		iniFileName = std::wstring(wdd) + std::wstring(wpd) + wfp + L".ini";
		iniFileNameLookFirst = std::wstring(wdp) + std::wstring(wpp) + wfp + L".ini";
	}

	// see if the INI file already exists in the plugin directory
	WIN32_FIND_DATAW findData = {0};
	SecureZeroMemory(&findData, sizeof(WIN32_FIND_DATAW));
	if (FindFirstFileT(iniFileNameLookFirst.c_str(), &findData) != INVALID_HANDLE_VALUE)
	{
		wchar_t tagsW[_MAX_PATH];
		GetPrivateProfileStringT(METADATA, nullptr, L"", tagsW, MAX_PATH - 1, iniFileNameLookFirst.c_str());
		if (wcscmp(tagsW, L""))
		{
			iniFileName = iniFileNameLookFirst;
			lngFileName = lngFileNameLookFirst;
			return;
		}
	}

	// see if the INI file already exists
	SecureZeroMemory(&findData, sizeof(WIN32_FIND_DATAW));
	if (FindFirstFileT(iniFileName.c_str(), &findData) == INVALID_HANDLE_VALUE)
	{
		// load default INI file string from resource
		HRSRC hRes = FindResource((HMODULE) hinst, MAKEINTRESOURCE(IDR_INIFILE), "FILE");
		HGLOBAL hGlobal = LoadResource((HMODULE) hinst, hRes);
		DWORD dwBytesToWrite = SizeofResource((HMODULE) hinst, hRes);
		DWORD dwBytesWritten = 0;
		BOOL err = FALSE;
		const char* iniStr = static_cast<const char*>(LockResource(hGlobal));
		if (iniStr)
		{
			HANDLE hFile = CreateFileT(iniFileName.c_str(), GENERIC_WRITE, 0,  nullptr, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, nullptr);
			if (hFile == INVALID_HANDLE_VALUE)
			{
				DisplayLastErrorMsgT(L"CreateFile", iniFileName.c_str(), MB_ICONWARNING | MB_OK);
			}
			else
			{
				while (dwBytesWritten < dwBytesToWrite)
				{
					if (FALSE == WriteFile(hFile, iniStr + dwBytesWritten, dwBytesToWrite - dwBytesWritten, &dwBytesWritten, nullptr))
					{
						DisplayLastErrorMsgT(L"WriteFile", iniFileName.c_str(), MB_ICONWARNING | MB_OK);
						err = TRUE;
						break;
					}
				}
				CloseHandle(hFile);
			}
		}
		UnlockResource(hGlobal);
		FreeResource(hRes);
		if (err == FALSE)
		{
			wchar_t msg[_MAX_PATH];
			wsprintfW(msg, L"New initialization file is \"%s\"", iniFileName.c_str());
			MessageBoxT(nullptr, msg, PLUGIN, MB_ICONINFORMATION | MB_OK);
		}
	}
}

int __stdcall ContentSetValue(char* FileName, int FieldIndex, int UnitIndex, int FieldType, void* FieldValue, int flags)
{
	wchar_t FileNameW[wdirtypemax];
	return ContentSetValueW(awfilenamecopy(FileNameW, FileName), FieldIndex, UnitIndex, FieldType, FieldValue, flags);
}

int __stdcall ContentSetValueW(wchar_t* FileName, int FieldIndex, int UnitIndex, int FieldType, void* FieldValue, int flags)
{
	if (FileName && (FieldIndex < fieldcount))
	{
		if (!loaded)
		{
			InitializeCriticalSection(&criti);
			EnterCriticalSection(&criti);
			loaded = TRUE;
			LeaveCriticalSection(&criti);
		}

		try
		{
			if (flags & setflags_first_attribute)
			{
				EnterCriticalSection(&criti);
				image = Exiv2::ImageFactory::open(FileName);
				LeaveCriticalSection(&criti);
				if (image->good())
				{
					image->readMetadata();
				}
				else
				{
					if (GetPrivateProfileIntT(SETTINGS, L"ShowErrors", 1, iniFileName.c_str()))
					{
						MessageBoxT(nullptr, L"Error: Image is not good", FileName, MB_ICONSTOP | MB_OK);
					}
					return ft_fileerror;
				}
			}

			EnterCriticalSection(&criti);
			fileCache.resetTimeStamp = FALSE;
			wcscpy(fileCache.fileName, FileName);
			HANDLE fh = CreateFileT(FileName, 0, FILE_SHARE_READ, 0, OPEN_EXISTING, 0, 0);
			if (fh != INVALID_HANDLE_VALUE)
			{
				fileCache.resetTimeStamp = GetFileTime(fh, &fileCache.ftCreate, &fileCache.ftAccess, &fileCache.ftWrite);
				CloseHandle(fh);
				if (!fileCache.resetTimeStamp)
				{
					DisplayLastErrorMsgT(L"GetFileTime", FileName, MB_ICONWARNING | MB_OK);
				}
			}
			else
			{
				DisplayLastErrorMsgT(L"CreateFile", FileName, MB_ICONWARNING | MB_OK);
			}
			LeaveCriticalSection(&criti);

			if (fieldIsJPGComment(FieldIndex))
			{
				switch (FieldType)
				{
					case ft_string:
						image->setComment((char *) FieldValue);
						break;

					case ft_stringw:
						image->setComment(Exiv2::ws2s((wchar_t *) FieldValue));
						break;

					default:
						return ft_nosuchfield;
				}
			}
			else if (fieldIsExif(FieldIndex))
			{
				Exiv2::ExifData &exifData = image->exifData();
				switch (FieldType)
				{
					case ft_numeric_32:
					case ft_numeric_64:
						exifData[keynames(FieldIndex)] = *((unsigned int *) FieldValue);
						break;

					case ft_multiplechoice:
						exifData[keynames(FieldIndex)] = (mcmap(FieldIndex)->at(UnitIndex)).first;
						break;

					case ft_string:
						exifData[keynames(FieldIndex)] = std::string((char *) FieldValue);
						break;

					case ft_stringw:
					{
						if (0 == strcmp("Exif.Photo.UserComment", keynames(FieldIndex)))
						{
							std::wstring s((wchar_t *) FieldValue);
							int slength = (int)s.length() + 1;
							int len = WideCharToMultiByte(CP_UTF8, 0, s.c_str(), slength, 0, 0, 0, 0);
							char* buf = new char[len];
							WideCharToMultiByte(CP_UTF8, 0, s.c_str(), slength, buf, len, 0, 0);
							exifData["Exif.Photo.UserComment"] = std::string("charset=\"Unicode\" ") + std::string(buf);
							delete[] buf;
						}
						else
						{
							std::wstring s((wchar_t *) FieldValue);
							int slength = (int)s.length() + 1;
							int len = WideCharToMultiByte(CP_UTF8, 0, s.c_str(), slength, 0, 0, 0, 0);
							char* buf = new char[len];
							WideCharToMultiByte(CP_UTF8, 0, s.c_str(), slength, buf, len, 0, 0);
							exifData[keynames(FieldIndex)] = std::string(buf);
							delete[] buf;
						}
						break;
					}

					case ft_date:
					{
						char dateStr[11] = "\0";
						wsprintfA(dateStr, "%4u:%02u:%02u", ((pdateformat)FieldValue)->wYear, ((pdateformat)FieldValue)->wMonth, ((pdateformat)FieldValue)->wDay);
						exifData[keynames(FieldIndex)] = std::string(dateStr);
						break;
					}

					case ft_datetime:
					{
						char dateStr[20] = "\0";
						SYSTEMTIME systime = {0};
						FILETIME ft = {0};
						FileTimeToLocalFileTime((const FILETIME*) FieldValue, &ft);
						FileTimeToSystemTime(&ft, &systime);
						wsprintfA(dateStr, "%4u:%02u:%02u %02u:%02u:%02u", systime.wYear, systime.wMonth, systime.wDay, systime.wHour, systime.wMinute, systime.wSecond);
						exifData[keynames(FieldIndex)] = std::string(dateStr);
						break;
					}

					default:
					{
						char msg[_MAX_PATH];
						wsprintfA(msg, "Error: Field type %d is not supported for EXIF tag \"%s\"", FieldType, keynames(FieldIndex));
						if (GetPrivateProfileIntT(SETTINGS, L"ShowErrors", 1, iniFileName.c_str()))
						{
							MessageBoxT(nullptr, Exiv2::s2ws(msg).c_str(), FileName, MB_ICONSTOP | MB_OK);
						}
						return ft_nosuchfield;
					}
				}
			}
			else if (fieldIsIptc(FieldIndex))
			{
				Exiv2::IptcData &iptcData = image->iptcData();
				switch (FieldType)
				{
					case ft_numeric_32:
						iptcData[keynames(FieldIndex)] = *((int *) FieldValue);
						break;

					case ft_multiplechoice:
						iptcData[keynames(FieldIndex)] = (mcmap(FieldIndex)->at(UnitIndex)).first;
						break;

					case ft_string:
					{
						Exiv2::IptcKey iptcKey(keynames(FieldIndex));
						if (Exiv2::IptcDataSets::dataSetRepeatable(iptcKey.tag(), iptcKey.record()))
						{
							while (TRUE)
							{
								Exiv2::IptcData::iterator iter = iptcData.findKey(iptcKey);
								if (iter == iptcData.end())
								{
									break;
								}
								else
								{
									iptcData.erase(iter);
								}
							}

							int Idx = 1;
							std::string Data = Token(std::string((char *) FieldValue), "\n\r", Idx);
							Exiv2::Value::AutoPtr value = Exiv2::Value::create(Exiv2::TypeInfo::typeId("String"));

							while (Data != "")
							{
								value->read(Data);
								iptcData.add(iptcKey, value.get());
								Data = Token(std::string((char *) FieldValue), "\n\r", ++Idx);
							}
						}
						else
						{
							iptcData[keynames(FieldIndex)] = std::string((char *) FieldValue);
						}
						break;
					}

					case ft_stringw:
					{
						std::wstring s((wchar_t *) FieldValue);
//						std::wstring s(L"???? ?????? & ???? ???????");
						int slength = (int)s.length() + 1;
						int len = WideCharToMultiByte(CP_UTF8, 0, s.c_str(), slength, 0, 0, 0, 0);
						char* buf = new char[len];
						WideCharToMultiByte(CP_UTF8, 0, s.c_str(), slength, buf, len, 0, 0);
						std::string r(buf);
						delete[] buf;
						iptcData[keynames(FieldIndex)] = r.c_str();
						break;
					}

					case ft_date:
					{
						char dateStr[11] = "\0";
						wsprintfA(dateStr, "%4u-%02u-%02u", ((pdateformat)FieldValue)->wYear, ((pdateformat)FieldValue)->wMonth, ((pdateformat)FieldValue)->wDay);
						iptcData[keynames(FieldIndex)] = std::string(dateStr);
						break;
					}

					case ft_time:
					{
						char timeStr[15] = "\0";
						TIME_ZONE_INFORMATION tzi = {0};
						GetTimeZoneInformation(&tzi);
						BOOL IsDayLight = TIME_ZONE_ID_DAYLIGHT == GetTimeZoneInformation(&tzi);
						BOOL Ahead;
						int HourOffset = 0;
						int MinOffset = 0;
						if (IsDayLight)
						{
							Ahead = (tzi.Bias + tzi.DaylightBias) < 0;
							HourOffset = Ahead ? -(tzi.Bias + tzi.DaylightBias)/60 : (tzi.Bias + tzi.DaylightBias)/60;
							MinOffset = Ahead ? -(tzi.Bias + tzi.DaylightBias)%60 : (tzi.Bias + tzi.DaylightBias)%60;
						}
						else
						{
							Ahead = tzi.Bias < 0;
							HourOffset = Ahead ? -tzi.Bias/60 : tzi.Bias/60;
							MinOffset = Ahead ? -tzi.Bias%60 : tzi.Bias%60;
						}
						wsprintfA(timeStr, "%02u:%02u:%02u%c%02u:%02u", ((ptimeformat)FieldValue)->wHour, ((ptimeformat)FieldValue)->wMinute, ((ptimeformat)FieldValue)->wSecond, Ahead ? '+' : '-', HourOffset, MinOffset);
						iptcData[keynames(FieldIndex)] = std::string(timeStr);
						break;
					}

					default:
					{
						char msg[_MAX_PATH];
						wsprintfA(msg, "Error: Field type %d is not supported for IPTC tag \"%s\"", FieldType, keynames(FieldIndex));
						if (GetPrivateProfileIntT(SETTINGS, L"ShowErrors", 1, iniFileName.c_str()))
						{
							MessageBoxT(nullptr, Exiv2::s2ws(msg).c_str(), FileName, MB_ICONSTOP | MB_OK);
						}
						return ft_nosuchfield;
					}
				}
			}
			else if (fieldIsXmp(FieldIndex))
			{
				Exiv2::XmpData &xmpData = image->xmpData();
				switch (FieldType)
				{
					case ft_numeric_32:
						xmpData[keynames(FieldIndex)] = *((int *) FieldValue);
						break;

					case ft_numeric_64:
						xmpData[keynames(FieldIndex)] = *((long long *) FieldValue);
						break;

					case ft_multiplechoice:
						xmpData[keynames(FieldIndex)] = (mcmap(FieldIndex)->at(UnitIndex)).first;
						break;

					case ft_string:
					case ft_stringw:
					{
						Exiv2::XmpKey xmpKey(keynames(FieldIndex));
						Exiv2::TypeId typeId = Exiv2::XmpProperties::propertyType(xmpKey);
						if (typeId == Exiv2::xmpBag || typeId == Exiv2::xmpSeq)
						{
							Exiv2::XmpData::iterator iter = xmpData.findKey(xmpKey);
							while (iter != xmpData.end())
							{
								xmpData.erase(iter);
								iter = xmpData.findKey(xmpKey);
							}
						}

						if (FieldType == ft_stringw)
						{
							std::wstring s((wchar_t *) FieldValue);
							int slength = (int)s.length() + 1;
							int len = WideCharToMultiByte(CP_UTF8, 0, s.c_str(), slength, 0, 0, 0, 0);
							char* buf = new char[len];
							WideCharToMultiByte(CP_UTF8, 0, s.c_str(), slength, buf, len, 0, 0);
							xmpData[keynames(FieldIndex)] = std::string(buf);
							delete[] buf;
						}
						else
						{
							xmpData[keynames(FieldIndex)] = std::string((char *) FieldValue);
						}
						break;
					}

					case ft_date:
					{
						char dateStr[11] = "\0";
						wsprintfA(dateStr, "%4u-%02u-%02u", ((pdateformat)FieldValue)->wYear, ((pdateformat)FieldValue)->wMonth, ((pdateformat)FieldValue)->wDay);
						xmpData[keynames(FieldIndex)] = std::string(dateStr);
						break;
					}

					case ft_time:
					{
						Exiv2::XmpKey xmpKey(keynames(FieldIndex));
						Exiv2::XmpData::iterator iter = xmpData.findKey(xmpKey);
						int Year = 0 , Month = 0, Day = 0;
						if (3 == sscanf(iter->value().toString().c_str(), "%4u-%02u-%02u", &Year, &Month, &Day))
						{
							char dateStr[26] = "\0";
							TIME_ZONE_INFORMATION tzi = {0};
							GetTimeZoneInformation(&tzi);
							BOOL IsDayLight = TIME_ZONE_ID_DAYLIGHT == GetTimeZoneInformation(&tzi);
							BOOL Ahead;
							int HourOffset = 0;
							int MinOffset = 0;
							if (IsDayLight)
							{
								Ahead = (tzi.Bias + tzi.DaylightBias) < 0;
								HourOffset = Ahead ? -(tzi.Bias + tzi.DaylightBias)/60 : (tzi.Bias + tzi.DaylightBias)/60;
								MinOffset = Ahead ? -(tzi.Bias + tzi.DaylightBias)%60 : (tzi.Bias + tzi.DaylightBias)%60;
							}
							else
							{
								Ahead = tzi.Bias < 0;
								HourOffset = Ahead ? -tzi.Bias/60 : tzi.Bias/60;
								MinOffset = Ahead ? -tzi.Bias%60 : tzi.Bias%60;
							}
							wsprintfA(dateStr, "%4u-%02u-%02uT%02u:%02u:%02u%c%02u:%02u", Year, Month, Day, ((ptimeformat)FieldValue)->wHour, ((ptimeformat)FieldValue)->wMinute, ((ptimeformat)FieldValue)->wSecond, Ahead ? '+' : '-', HourOffset, MinOffset);
							xmpData[keynames(FieldIndex)] = std::string(dateStr);
						}
						break;
					}

					case ft_datetime:
					{
						char dateStr[26] = "\0";
						SYSTEMTIME systime = {0};
						FILETIME ft = {0};
						TIME_ZONE_INFORMATION tzi = {0};
						FileTimeToLocalFileTime((const FILETIME*) FieldValue, &ft);
						FileTimeToSystemTime(&ft, &systime);
						BOOL IsDayLight = TIME_ZONE_ID_DAYLIGHT == GetTimeZoneInformation(&tzi);
						BOOL Ahead;
						int HourOffset = 0;
						int MinOffset = 0;
						if (IsDayLight)
						{
							Ahead = (tzi.Bias + tzi.DaylightBias) < 0;
							HourOffset = Ahead ? -(tzi.Bias + tzi.DaylightBias)/60 : (tzi.Bias + tzi.DaylightBias)/60;
							MinOffset = Ahead ? -(tzi.Bias + tzi.DaylightBias)%60 : (tzi.Bias + tzi.DaylightBias)%60;
						}
						else
						{
							Ahead = tzi.Bias < 0;
							HourOffset = Ahead ? -tzi.Bias/60 : tzi.Bias/60;
							MinOffset = Ahead ? -tzi.Bias%60 : tzi.Bias%60;
						}
						wsprintfA(dateStr, "%4u-%02u-%02uT%02u:%02u:%02u%c%02u:%02u", systime.wYear, systime.wMonth, systime.wDay, systime.wHour, systime.wMinute, systime.wSecond, Ahead ? '+' : '-', HourOffset, MinOffset);
						xmpData[keynames(FieldIndex)] = std::string(dateStr);
						break;
					}

					default:
					{
						char msg[_MAX_PATH];
						wsprintfA(msg, "Error: Field type %d is not supported for XMP tag \"%s\"", FieldType, keynames(FieldIndex));
						if (GetPrivateProfileIntT(SETTINGS, L"ShowErrors", 1, iniFileName.c_str()))
						{
							MessageBoxT(nullptr, Exiv2::s2ws(msg).c_str(), FileName, MB_ICONSTOP | MB_OK);
						}
						return ft_nosuchfield;
					}
				}
			}
			else
			{
				return ft_nosuchfield;
			}

			if (flags & setflags_last_attribute)
			{
				image->writeMetadata();

				if (fileCache.resetTimeStamp)
				{
					fh = CreateFileT(fileCache.fileName, GENERIC_WRITE, FILE_SHARE_WRITE, 0, OPEN_EXISTING, 0, 0);
					if (fh != INVALID_HANDLE_VALUE)
					{
						fileCache.resetTimeStamp = SetFileTime(fh, nullptr, nullptr, &fileCache.ftWrite);
						CloseHandle(fh);
						if (!fileCache.resetTimeStamp)
						{
							DisplayLastErrorMsgT(L"SetFileTime", fileCache.fileName, MB_ICONWARNING | MB_OK);
						}
					}
					else
					{
						DisplayLastErrorMsgT(L"CreateFile", fileCache.fileName, MB_ICONWARNING | MB_OK);
					}
				}
			}

			return ft_setsuccess;
		}
		catch (const XMP_Error& e)
		{
			if (GetPrivateProfileIntT(SETTINGS, L"ShowErrors", 1, iniFileName.c_str()))
			{
				MessageBoxT(nullptr, Exiv2::s2ws(e.GetErrMsg()).c_str(), FileName, MB_ICONSTOP | MB_OK);
			}
			return ft_fileerror;
		}
		catch (const Exiv2::WError& e)
		{
			if (GetPrivateProfileIntT(SETTINGS, L"ShowErrors", 1, iniFileName.c_str()))
			{
				MessageBoxT(nullptr, e.wwhat(), FileName, MB_ICONSTOP | MB_OK);
			}
			LeaveCriticalSection(&criti);
			return ft_fileerror;
		}
		catch (const std::exception& e)
		{
			if (GetPrivateProfileIntT(SETTINGS, L"ShowErrors", 1, iniFileName.c_str()))
			{
				MessageBoxT(nullptr, Exiv2::s2ws(e.what()).c_str(), FileName, MB_ICONSTOP | MB_OK);
			}
			return ft_fileerror;
		}
		catch(...)
		{
			return ft_fileerror;
		}
	}
	else
	{
		return ft_nosuchfield;
	}
}

void __stdcall ContentStopGetValue(char* FileName)
{
	wchar_t FileNameW[wdirtypemax];
	ContentStopGetValueW(awfilenamecopy(FileNameW, FileName));
}

void __stdcall ContentStopGetValueW(wchar_t* FileName)
{
	EnterCriticalSection(&criti);
	GetAborted = TRUE;
	LeaveCriticalSection(&criti);
}

int __stdcall ContentGetDefaultSortOrder(int FieldIndex)
{
	return ft_notsupported;
}

void __stdcall ContentSendStateInformation(int state, char* path)
{
}

void __stdcall ContentSendStateInformationW(int state, wchar_t* path)
{
}