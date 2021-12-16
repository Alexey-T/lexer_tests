
FUNCTION_BLOCK MaTypNr
	VAR_INPUT
		maTyp : USINT;
		maNr : USINT;
	END_VAR
	VAR_OUTPUT
		strMaTypNr : STRING[11];
	END_VAR
	VAR
		strTemp : STRING[4];
	END_VAR
END_FUNCTION_BLOCK

FUNCTION_BLOCK SpiderWebHis
	VAR_INPUT
		disturbance : ARRAY[0..20] OF BOOL;
		warning : ARRAY[0..10] OF BOOL;
		shiftNr : USINT;
		shiftStart : DATE_AND_TIME;
		machType : UDINT;
		machNr : USINT;
		mainState : USINT;
		line : USINT;
		textNr : ARRAY[0..20] OF INT;
		textWarnNr : ARRAY[0..10] OF INT;
	END_VAR
	VAR_OUTPUT
		spiderWebHis : spiderWebHis_s;
	END_VAR
	VAR
		actualSpiderWebHis : ARRAY[0..30] OF spiderWebHis_s;
		disturbanceFlag : ARRAY[0..20] OF BOOL;
		warningFlag : ARRAY[0..10] OF BOOL;
		DISTURBMAXIDX : USINT;
		WARNINGMAXIDX : USINT;
		i : USINT;
		j : USINT;
		status : UINT;
		actTime : DATE_AND_TIME;
		period : UDINT;
		zzDTGetTime00000 : DTGetTime;
	END_VAR
END_FUNCTION_BLOCK

FUNCTION_BLOCK CycleCount
	VAR_INPUT
		enInit : BOOL;
		enCycleCount : BOOL;
		tskCycleTimeMs : UINT;
		rollZeitMin : USINT;
		ProduktionAktiv : BOOL;
		VerlangtMaterial : BOOL;
	END_VAR
	VAR_OUTPUT
		CycleSum : UDINT;
	END_VAR
	VAR
		Bz : USINT;
		Index : USINT;
		i : USINT;
		cntProduktion : UDINT;
		cntCycle : UDINT;
		tmpCycleCount : ARRAY[0..19] OF UDINT;
		onFlag : BOOL;
	END_VAR
END_FUNCTION_BLOCK

FUNCTION_BLOCK StopGoRatio
	VAR_INPUT
		enInit : BOOL;
		enStopGo : BOOL;
		tskCycleTimeMs : UINT;
		enXStufenStopGo : REAL;
		rollZeitMin : USINT;
		ProduktionAktiv : BOOL;
		prodSoll : USINT;
		VerlangtMaterial : BOOL;
	END_VAR
	VAR_OUTPUT
		StopGoRatio : UINT;
	END_VAR
	VAR
		Bz : USINT;
		Index : USINT;
		j : USINT;
		jj : USINT;
		cntProduktion : UDINT;
		cntVerlangen : UDINT;
		tmpStopGo : ARRAY[0..19] OF UDINT;
		cntStopGo : UDINT;
	END_VAR
END_FUNCTION_BLOCK

FUNCTION_BLOCK SmartFeed
	VAR_INPUT
		init : BOOL;
		smartFeedOn : BOOL;
		actualPressure : UINT;
		actualPressureCycle : USINT;
		requestMat : BOOL;
		prodLevel : USINT;
		levelMax : USINT;
		tskCycleTimeMs : TIME;
		deadTimeMax : TIME;
		deadTimeCycle : USINT;
		pressureProgTime : UINT;
	END_VAR
	VAR_OUTPUT
		intercalPressure : UINT;
		pressureProgression : INT;
		deadTime : TIME;
	END_VAR
	VAR
		actualPressureFiltered : UINT;
		actualPressureOld : ARRAY[0..16] OF UINT;
		actualPressureSum : INT;
		pressureDif : ARRAY[0..100] OF INT;
		pressureDifSum : INT;
		i : USINT;
		maxIndex : USINT;
		actPos : ARRAY[0..2] OF USINT;
		feedFlag : BOOL;
		detectFlag : BOOL;
		deadTimeAct : TIME;
		deadTimeOld : ARRAY[0..15] OF TIME;
		deadTimeSum : TIME;
		maxIdx : USINT;
		intercalPressureInt : INT;
		deadTimeP : TIME;
		measureFlag : BOOL;
	END_VAR
END_FUNCTION_BLOCK
