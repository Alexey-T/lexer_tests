//===============================================================================
// Additional Parallel Programming examples for F#
// Copyright (c) Tomas Petricek, http://tomasp.net/blog
//===============================================================================
// This code released under the terms of the 
// Microsoft Public License (MS-PL, http://opensource.org/licenses/ms-pl.html.)
//===============================================================================

module Microsoft.Practices.ParallelGuideSamples.ImagePipeline.PipelineAgent

open System
open System.Threading
// Contains 'BlockingQueueAgent'
open FSharp.Control
// Contains 'loadImage', 'scaleImage' etc.
open Microsoft.Practices.ParallelGuideSamples.ImagePipeline.PipelineStandard 

let GaussianNoiseAmount = 50.0 

// ------------------------------------------------------------------------------
// A version using F# Mailbox Processor
// ------------------------------------------------------------------------------

let runMessagePassing fileNames sourceDir queueLength displayFn (cts:CancellationTokenSource) = 

  let loadedImages = new BlockingQueueAgent<_>(queueLength)
  let scaledImages = new BlockingQueueAgent<_>(queueLength)    
  let filteredImages = new BlockingQueueAgent<_>(queueLength)    

  // Image pipeline phase 1: Load images from disk and put them a queue.
  let loadImages = async {
    let clockOffset = Environment.TickCount
    let rec numbers n = seq { yield n; yield! numbers (n + 1) }
    for count, img in fileNames |> Seq.zip (numbers 0) do
      let info = loadImage img sourceDir count clockOffset
      do! loadedImages.AsyncAdd(info) }

  // Image pipeline phase 2: Scale to thumbnail size and render picture frame.
  let scalePipelinedImages = async {
    while true do 
      let! info = loadedImages.AsyncGet()
      scaleImage info
      do! scaledImages.AsyncAdd(info) }

  // Image pipeline phase 3: Filter images (give them a 
  // speckled appearance by adding Gaussian noise)
  let filterPipelinedImages = async {
    while true do 
      let! info = scaledImages.AsyncGet()
      filterImage info
      do! filteredImages.AsyncAdd(info) }

  // Image pipeline phase 4: Invoke the user-provided callback 
  // function (for example, to display the result in a UI)
  let displayPipelinedImages = 
    let rec loop count duration = async {
      let! info = filteredImages.AsyncGet()
      let displayStart = Environment.TickCount
      info.QueueCount1 <- loadedImages.Count
      info.QueueCount2 <- scaledImages.Count
      info.QueueCount3 <- filteredImages.Count
      displayImage info (count + 1) displayFn duration
      return! loop (count + 1) (Environment.TickCount - displayStart) }
    loop 0 0

  Async.Start(loadImages, cts.Token)
  Async.Start(scalePipelinedImages, cts.Token)
  Async.Start(filterPipelinedImages, cts.Token)

  try Async.RunSynchronously(displayPipelinedImages, cancellationToken = cts.Token)
  with :? OperationCanceledException -> () 
