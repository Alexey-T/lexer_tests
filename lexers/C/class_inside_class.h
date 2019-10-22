/*********************************************************************
	Rhapsody	: 8.1.1 
	Login		: Thomas.Beutlich
	Component	: MonitorSAT_MSVCDLL 
	Configuration 	: FMU
	Model Element	: MonitorIT
//!	Generated Date	: Mon, 2, Feb 2015  
	File Path	: MonitorSAT_MSVCDLL\FMU\MonitorIT.h
*********************************************************************/

#ifndef MonitorIT_H
#define MonitorIT_H

//## auto_generated
#include <oxf\oxf.h>
//## auto_generated
#include <..\Profiles\SysML\SIDefinitions.h>
//## auto_generated
#include "..\..\Stochastic\Stochastic.h"
//## auto_generated
#include <oxf\omreactive.h>
//## auto_generated
#include <oxf\state.h>
//## auto_generated
#include <oxf\event.h>
//## class MonitorIT
#include "floatFlowInterface.h"
//## package DeIcingPackage

//## class MonitorIT
class MonitorIT : public OMReactive, public floatFlowInterface {
public :

//#[ ignore
    //## package DeIcingPackage
    class in1_SP_C : public floatFlowInterface {
        ////    Constructors and destructors    ////
        
    public :
    
        //## auto_generated
        in1_SP_C();
        
        //## auto_generated
        virtual ~in1_SP_C();
        
        ////    Operations    ////
        
        //## auto_generated
        virtual void SetValue(float data, void * pCaller = NULL);
        
        //## auto_generated
        void connectMonitorIT(MonitorIT* part);
        
        //## auto_generated
        floatFlowInterface* getItsFloatFlowInterface();
        
        ////    Additional operations    ////
        
        //## auto_generated
        void setItsFloatFlowInterface(floatFlowInterface* p_floatFlowInterface);
    
    protected :
    
        //## auto_generated
        void cleanUpRelations();
        
        ////    Attributes    ////
        
        int _p_;		//## attribute _p_
        
        ////    Relations and components    ////
        
        floatFlowInterface* itsFloatFlowInterface;		//## link itsFloatFlowInterface
    };
    
    //## package DeIcingPackage
    class out1_SP_C : public floatFlowInterface {
        ////    Constructors and destructors    ////
        
    public :
    
        //## auto_generated
        out1_SP_C();
        
        //## auto_generated
        virtual ~out1_SP_C();
        
        ////    Operations    ////
        
        //## auto_generated
        virtual void SetValue(float data, void * pCaller = NULL);
        
        //## auto_generated
        floatFlowInterface* getItsFloatFlowInterface();
        
        //## auto_generated
        floatFlowInterface* getOutBound();
        
        ////    Additional operations    ////
        
        //## auto_generated
        void setItsFloatFlowInterface(floatFlowInterface* p_floatFlowInterface);
    
    protected :
    
        //## auto_generated
        void cleanUpRelations();
        
        ////    Attributes    ////
        
        int _p_;		//## attribute _p_
        
        ////    Relations and components    ////
        
        floatFlowInterface* itsFloatFlowInterface;		//## link itsFloatFlowInterface
    };
//#]

    ////    Constructors and destructors    ////
    
    //## auto_generated
    MonitorIT(IOxfActive* theActiveContext = 0);
    
    //## auto_generated
    ~MonitorIT();
    
    ////    Operations    ////
    
//#[ ignore
    void SetValue(float data, void * pCaller = NULL);
    
    void setIn1(float p_in1);
    
    void setOut1(float p_out1);
//#]

    ////    Additional operations    ////
    
    //## auto_generated
    in1_SP_C* getIn1_SP() const;
    
    //## auto_generated
    in1_SP_C* get_in1_SP() const;
    
    //## auto_generated
    out1_SP_C* getOut1_SP() const;
    
    //## auto_generated
    out1_SP_C* get_out1_SP() const;
    
    //## auto_generated
    float getIn1() const;
    
    //## auto_generated
    float getNoiseRangeMax() const;
    
    //## auto_generated
    void setNoiseRangeMax(float p_noiseRangeMax);
    
    //## auto_generated
    float getNoiseRangeMin() const;
    
    //## auto_generated
    void setNoiseRangeMin(float p_noiseRangeMin);
    
    //## auto_generated
    float getOut1() const;
    
    //## auto_generated
    int getPeriod() const;
    
    //## auto_generated
    void setPeriod(int p_period);
    
    //## auto_generated
    long getRndSeed() const;
    
    //## auto_generated
    void setRndSeed(long p_rndSeed);
    
    //## auto_generated
    virtual bool startBehavior();

protected :

    //## auto_generated
    void initRelations();
    
    //## auto_generated
    void initStatechart();
    
    //## auto_generated
    void cancelTimeouts();
    
    //## auto_generated
    bool cancelTimeout(const IOxfTimeout* arg);
    
    ////    Attributes    ////
    
    float in1;		//## attribute in1
    
    float noiseRangeMax;		//## attribute noiseRangeMax
    
    float noiseRangeMin;		//## attribute noiseRangeMin
    
    float out1;		//## attribute out1
    
    int period;		//## attribute period
    
    long rndSeed;		//## attribute rndSeed
    
    ////    Relations and components    ////
    
//#[ ignore
    in1_SP_C in1_SP;
    
    out1_SP_C out1_SP;
//#]

    ////    Framework operations    ////

public :

    // rootState:
    //## statechart_method
    inline bool rootState_IN() const;
    
    //## statechart_method
    virtual void rootState_entDef();
    
    //## statechart_method
    virtual IOxfReactive::TakeEventStatus rootState_processEvent();
    
    // state_0:
    //## statechart_method
    inline bool state_0_IN() const;
    
    ////    Framework    ////

protected :

//#[ ignore
    enum MonitorIT_Enum {
        OMNonState = 0,
        state_0 = 1
    };
    
    int rootState_subState;
    
    int rootState_active;
    
    IOxfTimeout* rootState_timeout;
//#]
};

inline bool MonitorIT::rootState_IN() const {
    return true;
}

inline bool MonitorIT::state_0_IN() const {
    return rootState_subState == state_0;
}

#endif
/*********************************************************************
	File Path	: MonitorSAT_MSVCDLL\FMU\MonitorIT.h
*********************************************************************/
