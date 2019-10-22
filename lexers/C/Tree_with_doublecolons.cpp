/********************************************************************
	Rhapsody	: 8.1.1 
	Login		: Thomas.Beutlich
	Component	: MonitorSAT_MSVCDLL 
	Configuration 	: FMU
	Model Element	: MonitorIT
//!	Generated Date	: Mon, 2, Feb 2015  
	File Path	: MonitorSAT_MSVCDLL\FMU\MonitorIT.cpp
*********************************************************************/

//## auto_generated
#include <oxf\omthread.h>
//## auto_generated
#include "MonitorIT.h"
//## package DeIcingPackage

//## class MonitorIT
//#[ ignore
MonitorIT::in1_SP_C::in1_SP_C() : _p_(0) {
    itsFloatFlowInterface = NULL;
}

MonitorIT::in1_SP_C::~in1_SP_C() {
    cleanUpRelations();
}

void MonitorIT::in1_SP_C::SetValue(float data, void * pCaller) {
    
    if (itsFloatFlowInterface != NULL) {
        itsFloatFlowInterface->SetValue(data,this);
    }
    
}

void MonitorIT::in1_SP_C::connectMonitorIT(MonitorIT* part) {
    setItsFloatFlowInterface(part);
    
}

floatFlowInterface* MonitorIT::in1_SP_C::getItsFloatFlowInterface() {
    return this;
}

void MonitorIT::in1_SP_C::setItsFloatFlowInterface(floatFlowInterface* p_floatFlowInterface) {
    itsFloatFlowInterface = p_floatFlowInterface;
}

void MonitorIT::in1_SP_C::cleanUpRelations() {
    if(itsFloatFlowInterface != NULL)
        {
            itsFloatFlowInterface = NULL;
        }
}

MonitorIT::out1_SP_C::out1_SP_C() : _p_(0) {
    itsFloatFlowInterface = NULL;
}

MonitorIT::out1_SP_C::~out1_SP_C() {
    cleanUpRelations();
}

void MonitorIT::out1_SP_C::SetValue(float data, void * pCaller) {
    
    if (itsFloatFlowInterface != NULL) {
        itsFloatFlowInterface->SetValue(data,pCaller);
    }
    
}

floatFlowInterface* MonitorIT::out1_SP_C::getItsFloatFlowInterface() {
    return this;
}

floatFlowInterface* MonitorIT::out1_SP_C::getOutBound() {
    return this;
}

void MonitorIT::out1_SP_C::setItsFloatFlowInterface(floatFlowInterface* p_floatFlowInterface) {
    itsFloatFlowInterface = p_floatFlowInterface;
}

void MonitorIT::out1_SP_C::cleanUpRelations() {
    if(itsFloatFlowInterface != NULL)
        {
            itsFloatFlowInterface = NULL;
        }
}
//#]

MonitorIT::MonitorIT(IOxfActive* theActiveContext) : in1(0), noiseRangeMax(+0.0005), noiseRangeMin(-0.0005), out1(0), period(1000), rndSeed(0) {
    setActiveContext(theActiveContext, false);
    initRelations();
    initStatechart();
}

MonitorIT::~MonitorIT() {
    cancelTimeouts();
}

//#[ ignore
void MonitorIT::SetValue(float data, void * pCaller) {
    if (pCaller == (void *)get_in1_SP()) {
        setIn1(data);
    }
}

void MonitorIT::setIn1(float p_in1) {
    if (in1 != p_in1) {
        in1 = p_in1;
        FLOW_DATA_RECEIVE("in1", in1, x2String);
    }
    
}

void MonitorIT::setOut1(float p_out1) {
    if (out1 != p_out1)  {
        out1 = p_out1;
        FLOW_DATA_SEND(out1, out1_SP, SetValue, x2String);
    }
}
//#]

MonitorIT::in1_SP_C* MonitorIT::getIn1_SP() const {
    return (MonitorIT::in1_SP_C*) &in1_SP;
}

MonitorIT::in1_SP_C* MonitorIT::get_in1_SP() const {
    return (MonitorIT::in1_SP_C*) &in1_SP;
}

MonitorIT::out1_SP_C* MonitorIT::getOut1_SP() const {
    return (MonitorIT::out1_SP_C*) &out1_SP;
}

MonitorIT::out1_SP_C* MonitorIT::get_out1_SP() const {
    return (MonitorIT::out1_SP_C*) &out1_SP;
}

float MonitorIT::getIn1() const {
    return in1;
}

float MonitorIT::getNoiseRangeMax() const {
    return noiseRangeMax;
}

void MonitorIT::setNoiseRangeMax(float p_noiseRangeMax) {
    noiseRangeMax = p_noiseRangeMax;
}

float MonitorIT::getNoiseRangeMin() const {
    return noiseRangeMin;
}

void MonitorIT::setNoiseRangeMin(float p_noiseRangeMin) {
    noiseRangeMin = p_noiseRangeMin;
}

float MonitorIT::getOut1() const {
    return out1;
}

int MonitorIT::getPeriod() const {
    return period;
}

void MonitorIT::setPeriod(int p_period) {
    period = p_period;
}

long MonitorIT::getRndSeed() const {
    return rndSeed;
}

void MonitorIT::setRndSeed(long p_rndSeed) {
    rndSeed = p_rndSeed;
}

bool MonitorIT::startBehavior() {
    bool done = false;
    done = OMReactive::startBehavior();
    return done;
}

void MonitorIT::initRelations() {
    if (get_in1_SP() != NULL) {
        get_in1_SP()->connectMonitorIT(this);
    }
}

void MonitorIT::initStatechart() {
    rootState_subState = OMNonState;
    rootState_active = OMNonState;
    rootState_timeout = NULL;
    
}

void MonitorIT::cancelTimeouts() {
    cancel(rootState_timeout);
}

bool MonitorIT::cancelTimeout(const IOxfTimeout* arg) {
    bool res = false;
    if(rootState_timeout == arg)
        {
            rootState_timeout = NULL;
            res = true;
        }
    return res;
}

void MonitorIT::rootState_entDef() {
    {
        //#[ transition 0 
        setSeed(rndSeed);
        //#]
        rootState_subState = state_0;
        rootState_active = state_0;
        //#[ state state_0.(Entry) 
        setOut1(in1+UniformRandomReal(noiseRangeMin,noiseRangeMax));
        //#]
        rootState_timeout = scheduleTimeout(period, NULL);
    }
}

IOxfReactive::TakeEventStatus MonitorIT::rootState_processEvent() {
    IOxfReactive::TakeEventStatus res = eventNotConsumed;
    // State state_0
    if(rootState_active == state_0)
        {
            if(IS_EVENT_TYPE_OF(OMTimeoutEventId))
                {
                    if(getCurrentEvent() == rootState_timeout)
                        {
                            cancel(rootState_timeout);
                            rootState_subState = state_0;
                            rootState_active = state_0;
                            //#[ state state_0.(Entry) 
                            setOut1(in1+UniformRandomReal(noiseRangeMin,noiseRangeMax));
                            //#]
                            rootState_timeout = scheduleTimeout(period, NULL);
                            res = eventConsumed;
                        }
                }
            
        }
    return res;
}

/*********************************************************************
	File Path	: MonitorSAT_MSVCDLL\FMU\MonitorIT.cpp
*********************************************************************/
