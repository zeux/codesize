module Process

open System.Diagnostics
open System.IO
open System.Threading

let private pread (proc: Process) =
    seq {
        // read stdout
        use reader = proc.StandardOutput
        while not reader.EndOfStream do
            let s = reader.ReadLine()
            if s <> null then yield s

        // check process exit code
        proc.WaitForExit()
        if proc.ExitCode <> 0 then
            failwithf "Process %s %s exited with error %d" proc.StartInfo.FileName proc.StartInfo.Arguments proc.ExitCode
    }

// Launch process and return standard output
let popen exe arguments =
    let startInfo =
        ProcessStartInfo(exe, arguments, CreateNoWindow = true, UseShellExecute = false,
            RedirectStandardOutput = true)

    let proc = Process.Start(startInfo)
    pread proc

// Launch process, print some data to standard input and return standard output
let popen2 exe arguments stdin =
    let startInfo =
        ProcessStartInfo(exe, arguments, CreateNoWindow = true, UseShellExecute = false,
            RedirectStandardInput = true, RedirectStandardOutput = true)

    let proc = Process.Start(startInfo)

    // Write to stdin from another thread because pipe buffer has a limited size
    ThreadPool.QueueUserWorkItem(fun _ ->
        for line : string in stdin do proc.StandardInput.WriteLine(line)
        proc.StandardInput.Close()) |> ignore

    pread proc