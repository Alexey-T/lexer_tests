//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

namespace Microsoft.Practices.ParallelGuideSamples.ADash.ViewModel

open System
open System.Windows.Input
open Microsoft.Practices.ParallelGuideSamples.ADash.BusinessObjects

[<AbstractClass>]
type AbstractMainWindowViewModel() =
    
    // Events (since the type is used only from F#, we expose
    // events as F#-compatible events; if this was used from C#, 
    // add the [<CLIEvent>] attribute to compile them as C# events)
    abstract RequestAnalyzed : IEvent<EventHandler, EventArgs>
    abstract RequestAnalyzedHistorical : IEvent<EventHandler, EventArgs>
    abstract RequestClose : IEvent<EventHandler, EventArgs>
    abstract RequestFedHistorical : IEvent<EventHandler, EventArgs>
    abstract RequestMerged : IEvent<EventHandler, EventArgs>
    abstract RequestModeled : IEvent<EventHandler, EventArgs>
    abstract RequestModeledHistorical : IEvent<EventHandler, EventArgs>
    abstract RequestNasdaq : IEvent<EventHandler, EventArgs>
    abstract RequestNormalized : IEvent<EventHandler, EventArgs>
    abstract RequestNormalizedHistorical : IEvent<EventHandler, EventArgs>
    abstract RequestNyse : IEvent<EventHandler, EventArgs>
    abstract RequestRecommendation : IEvent<EventHandler, EventArgs>

    // Publicly readable data properties
    abstract StatusTextBoxText : string with get, set
    abstract IsCancelEnabled : bool
    abstract IsCalculateEnabled : bool

    abstract NyseMarketData : StockDataCollection option with get, set
    abstract NasdaqMarketData : StockDataCollection option with get, set
    abstract MergedMarketData : StockDataCollection option with get, set
    abstract NormalizedMarketData : StockDataCollection option with get, set
    abstract FedHistoricalData : StockDataCollection option with get, set
    abstract NormalizedHistoricalData : StockDataCollection option with get, set
    abstract AnalyzedStockData : StockAnalysisCollection option with get, set
    abstract AnalyzedHistoricalData : StockAnalysisCollection option with get, set
    abstract ModeledMarketData : MarketModel option with get, set
    abstract ModeledHistoricalData : MarketModel option with get, set
    abstract Recommendation : MarketRecommendation option with get, set

    // Commands
    abstract CloseCommand : ICommand
    abstract CalculateCommand : ICommand
    abstract CancelCommand : ICommand

    abstract NyseCommand : ICommand
    abstract NasdaqCommand : ICommand
    abstract MergedCommand : ICommand
    abstract NormalizedCommand : ICommand
    abstract FedHistoricalCommand : ICommand
    abstract NormalizedHistoricalCommand : ICommand
    abstract AnalyzedCommand : ICommand
    abstract AnalyzedHistoricalCommand : ICommand
    abstract ModeledCommand : ICommand
    abstract ModeledHistoricalCommand : ICommand
    abstract RecommendationCommand : ICommand
