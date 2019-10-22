//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

namespace Microsoft.Practices.ParallelGuideSamples.ADash

open System
open System.Threading.Tasks
open Microsoft.Practices.ParallelGuideSamples.ADash.BusinessObjects

/// Abstract class that allows the ViewModel component to react to completion
/// of a task or asynchronous workflow running in background.
/// This type is returned from AnalysisEnginge from 'DoAnalysis' method
[<AbstractClass>]
type AnalysisProcess() = 

    let guiDispatch = Windows.Threading.Dispatcher.CurrentDispatcher

    /// Trigger event on the GUI thread using a captured dispatcher
    let triggerGuiEvent (e:Event<_>) v = 
        guiDispatch.Invoke(new Action(fun () -> 
              e.Trigger(v) ), [| |]) |> ignore

    // Create events that are used to notify the 
    // caller about partial results of the computation
    let loadNyseDataEvt = new Event<StockDataCollection>()
    let loadNasdaqDataEvt = new Event<StockDataCollection>()
    let mergeMarketDataEvt = new Event<StockDataCollection>()
    let normalizeMarketDataEvt = new Event<StockDataCollection>()
    let loadFedHistoricalDataEvt = new Event<StockDataCollection>()
    let normalizeHistoricalDataEvt = new Event<StockDataCollection>()
    let analyzeMarketDataEvt = new Event<StockAnalysisCollection>()
    let analyzeHistoricalDataEvt = new Event<StockAnalysisCollection>()
    let modelMarketDataEvt = new Event<MarketModel>()
    let modelHistoricalDataEvt = new Event<MarketModel>()
    let compareModelsEvt = new Event<MarketRecommendation option>()
    let errorHandlerEvt = new Event<unit>()

    /// Start the analysis (to be used after handlers are registered)
    abstract Start : unit -> unit
    /// Try to cancel the process (if it has been already started)
    abstract TryCancel : unit -> unit

    member x.LoadNyseData = loadNyseDataEvt.Publish
    member x.LoadNasdaqData = loadNasdaqDataEvt.Publish
    member x.MergeMarketData = mergeMarketDataEvt.Publish
    member x.NormalizeMarketData = normalizeMarketDataEvt.Publish
    member x.LoadFedHistoricalData = loadFedHistoricalDataEvt.Publish
    member x.NormalizeHistoricalData = normalizeHistoricalDataEvt.Publish
    member x.AnalyzeMarketData = analyzeMarketDataEvt.Publish
    member x.AnalyzeHistoricalData = analyzeHistoricalDataEvt.Publish
    member x.ModelMarketData = modelMarketDataEvt.Publish
    member x.ModelHistoricalData = modelHistoricalDataEvt.Publish
    member x.CompareModels = compareModelsEvt.Publish
    member x.ErrorHandler = errorHandlerEvt.Publish 

    member x.TriggerLoadNyseData(v) = triggerGuiEvent loadNyseDataEvt v
    member x.TriggerLoadNasdaqData(v) = triggerGuiEvent loadNasdaqDataEvt v
    member x.TriggerMergeMarketData(v) = triggerGuiEvent mergeMarketDataEvt v
    member x.TriggerNormalizeMarketData(v) = triggerGuiEvent normalizeMarketDataEvt v
    member x.TriggerLoadFedHistoricalData(v) = triggerGuiEvent loadFedHistoricalDataEvt v
    member x.TriggerNormalizeHistoricalData(v) = triggerGuiEvent normalizeHistoricalDataEvt v
    member x.TriggerAnalyzeMarketData(v) = triggerGuiEvent analyzeMarketDataEvt v
    member x.TriggerAnalyzeHistoricalData(v) = triggerGuiEvent analyzeHistoricalDataEvt v
    member x.TriggerModelMarketData(v) = triggerGuiEvent modelMarketDataEvt v
    member x.TriggerModelHistoricalData(v) = triggerGuiEvent modelHistoricalDataEvt v
    member x.TriggerCompareModels(v) = triggerGuiEvent compareModelsEvt v
    member x.TriggerErrorHandler(v) = triggerGuiEvent errorHandlerEvt v