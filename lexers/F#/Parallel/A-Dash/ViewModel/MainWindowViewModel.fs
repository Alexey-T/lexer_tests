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
open System.ComponentModel
open System.Threading
open System.Threading.Tasks
open System.Windows.Input
open Microsoft.Practices.ParallelGuideSamples.ADash
open Microsoft.Practices.ParallelGuideSamples.ADash.BusinessObjects

type State = 
    | Ready = 0 
    | Calculating = 1 
    | Canceling = 2

type MainWindowViewModel(engine:IAnalysisEngine) as x = 
    inherit AbstractMainWindowViewModel()

    // --------------------------------------------------------------------------
    // Private fields (state & events) of the view model
    // --------------------------------------------------------------------------
    
    // Currently running analysis process
    let mutable currentProcess : AnalysisProcess option = None

    // Run the sequential implementation?
    let mutable runSequential = false

    // View model's current mode of operation
    let mutable modelState = State.Ready

    // Results of analysis or null if not yet computed
    let mutable nyseMarketData : StockDataCollection option = None
    let mutable nasdaqMarketData : StockDataCollection option = None
    let mutable mergedMarketData : StockDataCollection option = None
    let mutable normalizedMarketData : StockDataCollection option = None
    let mutable fedHistoricalData : StockDataCollection option = None
    let mutable normalizedHistoricalData : StockDataCollection option = None
    let mutable analyzedStockData : StockAnalysisCollection option = None
    let mutable analyzedHistoricalData : StockAnalysisCollection option = None
    let mutable modeledMarketData : MarketModel option = None
    let mutable modeledHistoricalData : MarketModel option = None
    let mutable recommendation : MarketRecommendation option = None

    // Status string that appears in the UI
    let mutable statusText = "" 

    // Raised when a public property of this class changes
    let propertyChanged = new Event<PropertyChangedEventHandler, PropertyChangedEventArgs>()

    // Raised when the corresponding command is invoked. 
    let requestClose = new Event<_, _>()
    let requestNyse = new Event<_, _>()
    let requestNasdaq = new Event<_, _>()
    let requestMerged = new Event<_, _>()
    let requestNormalized = new Event<_, _>()
    let requestFedHistorical = new Event<_, _>()
    let requestNormalizedHistorical = new Event<_, _>()
    let requestAnalyzed = new Event<_, _>()
    let requestAnalyzedHistorical = new Event<_, _>()
    let requestModeled = new Event<_, _>()
    let requestModeledHistorical = new Event<_, _>()
    let requestRecommendation = new Event<_, _>()

    // --------------------------------------------------------------------------
    // Command objects exposed by this view model for use by the view
    // --------------------------------------------------------------------------
    
    let makeCommand (evt:Event<_, _>) reader = 
        lazy Command((fun _ -> evt.Trigger(x, EventArgs.Empty)), (fun _ -> reader() <> None))

    let closeCommand = lazy Command(ignore >> x.OnRequestClose)
    let calculateCommand = lazy Command(ignore >> x.OnRequestCalculate)
    let cancelCommand = lazy Command(ignore >> x.OnRequestCancel, fun _ -> modelState = State.Calculating)
    let nyseCommand = makeCommand requestNyse (fun _ -> nyseMarketData)
    let nasdaqCommand = makeCommand requestNasdaq (fun _ -> nasdaqMarketData)
    let mergedCommand = makeCommand requestMerged (fun _ -> mergedMarketData)
    let normalizedCommand = makeCommand requestNormalized (fun _ -> normalizedMarketData)
    let fedHistoricalCommand = makeCommand requestFedHistorical (fun _ -> fedHistoricalData)
    let normalizedHistoricalCommand = makeCommand requestNormalizedHistorical (fun _ -> normalizedHistoricalData)
    let analyzedCommand = makeCommand requestAnalyzed (fun _ -> analyzedStockData)
    let analyzedHistoricalCommand = makeCommand requestAnalyzedHistorical (fun _ -> analyzedHistoricalData)
    let modeledCommand = makeCommand requestModeled (fun _ -> modeledMarketData)
    let modeledHistoricalCommand = makeCommand requestModeledHistorical (fun _ -> modeledHistoricalData)
    let recommendationCommand = makeCommand requestRecommendation (fun _ -> recommendation)

    // --------------------------------------------------------------------------
    // Implementation of the view model interface
    // --------------------------------------------------------------------------
    
    // Public events of the ViewModel

    override x.RequestNyse = requestNyse.Publish
    override x.RequestNasdaq = requestNasdaq.Publish
    override x.RequestMerged = requestMerged.Publish
    override x.RequestNormalized = requestNormalized.Publish
    override x.RequestFedHistorical = requestFedHistorical.Publish
    override x.RequestNormalizedHistorical = requestNormalizedHistorical.Publish
    override x.RequestAnalyzed = requestAnalyzed.Publish
    override x.RequestAnalyzedHistorical = requestAnalyzedHistorical.Publish
    override x.RequestModeled = requestModeled.Publish
    override x.RequestModeledHistorical = requestModeledHistorical.Publish
    override x.RequestRecommendation = requestRecommendation.Publish

    // Public data properties of the ViewModel

    override x.IsCancelEnabled = 
        (modelState = State.Calculating ||
          modelState = State.Canceling)

    override x.IsCalculateEnabled =
        modelState <> State.Calculating

    override x.StatusTextBoxText 
        with get() = statusText
        and set(value) = 
            statusText <- value
            x.OnPropertyChanged("StatusTextBoxText")

    override x.NyseMarketData
        with get() = nyseMarketData
        and set(value) =
            nyseMarketData <- value
            x.OnPropertyChanged("NyseMarketData")
            nyseCommand.Value.NotifyExecuteChanged()

    override x.NasdaqMarketData
        with get() = nasdaqMarketData
        and set(value) =
            nasdaqMarketData <- value
            x.OnPropertyChanged("NasdaqMarketData")
            nasdaqCommand.Value.NotifyExecuteChanged()

    override x.MergedMarketData
        with get() = mergedMarketData
        and set(value) =
            mergedMarketData <- value
            x.OnPropertyChanged("MergedMarketData")
            mergedCommand.Value.NotifyExecuteChanged()

    override x.NormalizedMarketData
        with get() = normalizedMarketData
        and set(value) =
            normalizedMarketData <- value
            x.OnPropertyChanged("NormalizedMarketData")
            normalizedCommand.Value.NotifyExecuteChanged()

    override x.FedHistoricalData
        with get() = fedHistoricalData
        and set(value) =
            fedHistoricalData <- value
            x.OnPropertyChanged("FedHistoricalData")
            fedHistoricalCommand.Value.NotifyExecuteChanged()

    override x.NormalizedHistoricalData
        with get() = normalizedHistoricalData
        and set(value) =
            normalizedHistoricalData <- value
            x.OnPropertyChanged("NormalizedHistoricalData")
            normalizedHistoricalCommand.Value.NotifyExecuteChanged()

    override x.AnalyzedStockData
        with get() = analyzedStockData
        and set(value) =
            analyzedStockData <- value
            x.OnPropertyChanged("AnalyzedMarketData")
            analyzedCommand.Value.NotifyExecuteChanged()

    override x.AnalyzedHistoricalData
        with get() = analyzedHistoricalData
        and set(value) =
            analyzedHistoricalData <- value
            x.OnPropertyChanged("AnalyzedHistoricalData")
            analyzedHistoricalCommand.Value.NotifyExecuteChanged()

    override x.ModeledMarketData
        with get() = modeledMarketData
        and set(value) =
            modeledMarketData <- value
            x.OnPropertyChanged("ModeledMarketData")
            modeledCommand.Value.NotifyExecuteChanged()

    override x.ModeledHistoricalData
        with get() = modeledHistoricalData
        and set(value) =
            modeledHistoricalData <- value
            x.OnPropertyChanged("ModeledHistoricalData")
            modeledHistoricalCommand.Value.NotifyExecuteChanged()

    override x.Recommendation
        with get() = recommendation
        and set(value) =
            recommendation <- value
            x.OnPropertyChanged("Recommendation")
            recommendationCommand.Value.NotifyExecuteChanged()

    member x.IsSequentialSelected
        with get() = runSequential
        and set(value) = 
            runSequential <- value
            x.OnPropertyChanged("IsSequentialSelected")
            x.OnPropertyChanged("IsAsynchronousSelected")

    member x.IsAsynchronousSelected
        with get() = not runSequential
        and set(value) = 
            runSequential <- not value
            x.OnPropertyChanged("IsSequentialSelected")
            x.OnPropertyChanged("IsAsynchronousSelected")


    // Raised when the associated view 
    // window should be closed (i.e. application shutdown)
    override x.RequestClose = requestClose.Publish

    // Publicly exposed commands for working with the ViewModel

    override x.CloseCommand = closeCommand.Value :> ICommand
    override x.CalculateCommand = calculateCommand.Value :> ICommand
    override x.CancelCommand = cancelCommand.Value :> ICommand
    override x.NyseCommand = nyseCommand.Value :> ICommand
    override x.NasdaqCommand = nasdaqCommand.Value :> ICommand
    override x.MergedCommand = mergedCommand.Value :> ICommand
    override x.NormalizedCommand = normalizedCommand.Value :> ICommand
    override x.FedHistoricalCommand = fedHistoricalCommand.Value :> ICommand
    override x.NormalizedHistoricalCommand = normalizedHistoricalCommand.Value :> ICommand
    override x.AnalyzedCommand = analyzedCommand.Value :> ICommand
    override x.AnalyzedHistoricalCommand = analyzedHistoricalCommand.Value :> ICommand
    override x.ModeledCommand = modeledCommand.Value :> ICommand
    override x.ModeledHistoricalCommand = modeledHistoricalCommand.Value :> ICommand
    override x.RecommendationCommand = recommendationCommand.Value :> ICommand

    member x.ModelState 
        with get() = modelState
        and set(value) =
            modelState <- value
            // issue notification of property change, including derived properties
            // and commands whose "CanExecute" status depend on model state.
            x.OnPropertyChanged("IsCancelEnabled")
            x.OnPropertyChanged("IsCalculateEnabled")
            calculateCommand.Value.NotifyExecuteChanged()
            cancelCommand.Value.NotifyExecuteChanged()
    
    // --------------------------------------------------------------------------
    // Command implementations
    // --------------------------------------------------------------------------

    /// Start the calculation in background and register notifications
    member x.OnRequestCalculate() =
        // Initialize the result properties to null
        x.ResetResultProperties()
        // Place the view model into calculation mode
        x.ModelState <- State.Calculating

        if runSequential then 
            // Update the property containing the status text
            x.StatusTextBoxText <- "...sequential..."
            // Create analysis computation
            currentProcess <- Some(engine.DoSequentialAnalysis())
        else
            // Update the property containing the status text
            x.StatusTextBoxText <- "...asynchronous..."
            // Create analysis computation
            currentProcess <- Some(engine.DoAsyncAnalysis())

        // Add continuations so that view model properties are updated when each subtask completes
        x.AddNotifications(currentProcess.Value)
        // Start the analysis
        currentProcess.Value.Start()
 

    /// Register notifications with tasks/asyncs so that the view model's 
    /// properties are updated when each task has results available for display.
    member x.AddNotifications (tasks:AnalysisProcess) =
        tasks.LoadNyseData.Add(fun t -> x.NyseMarketData <- Some(t))
        tasks.LoadNasdaqData.Add(fun t -> x.NasdaqMarketData <- Some(t))
        tasks.LoadFedHistoricalData.Add(fun t -> x.FedHistoricalData <- Some(t))
        tasks.MergeMarketData.Add(fun t -> x.MergedMarketData <- Some(t))
        tasks.NormalizeHistoricalData.Add(fun t -> x.NormalizedHistoricalData <- Some(t))
        tasks.NormalizeMarketData.Add(fun t -> x.NormalizedMarketData <- Some(t))
        tasks.AnalyzeHistoricalData.Add(fun t -> x.AnalyzedHistoricalData <- Some(t))
        tasks.AnalyzeMarketData.Add(fun t -> x.AnalyzedStockData <- Some(t))
        tasks.ModelHistoricalData.Add(fun t -> x.ModeledHistoricalData <- Some(t))
        tasks.ModelMarketData.Add(fun t -> x.ModeledMarketData <- Some(t))
        tasks.CompareModels.Add(fun t ->
            x.Recommendation <- t
            x.StatusTextBoxText <- match x.Recommendation with None -> "Canceled" | Some(v) -> v.Value
            x.ModelState <- State.Ready )
        tasks.ErrorHandler.Add(fun () ->
            x.StatusTextBoxText <- "Error"
            x.ModelState <- State.Ready)

    /// Set default values of results (called before starting calculation)
    member x.ResetResultProperties() =
        x.NyseMarketData <- None
        x.NasdaqMarketData <- None
        x.MergedMarketData <- None
        x.NormalizedMarketData <- None
        x.FedHistoricalData <- None
        x.NormalizedHistoricalData <- None
        x.AnalyzedStockData <- None
        x.AnalyzedHistoricalData <- None
        x.ModeledMarketData <- None
        x.ModeledHistoricalData <- None
        x.Recommendation <- None

    /// Close the application - cancel analysis
    member x.OnRequestClose() =
        currentProcess |> Option.iter (fun p -> p.TryCancel())
        requestClose.Trigger(x, EventArgs.Empty)

    /// Cancel analysis and wait until it finishes
    member x.OnRequestCancel() =
        currentProcess |> Option.iter (fun p -> p.TryCancel())
        x.ModelState <- State.Canceling
        x.StatusTextBoxText <- "Canceling..."

    /// Value of a property has been changed (INotifyPropertyChanged)
    member x.OnPropertyChanged(name) =
        propertyChanged.Trigger(x, new PropertyChangedEventArgs(name))

    // --------------------------------------------------------------------------
    // Implementation of INotifyPropertyChanged and IDisposable
    // --------------------------------------------------------------------------
    
    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member x.PropertyChanged = propertyChanged.Publish