using DTS.Base;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;

/// <summary>
/// "'ContentManagerMigration'_yyyy-MM-dd'.log'"
/// </summary>

namespace CMDosDumper
{
    public class LightLogger : ILogger
    {
        private Dictionary<LogLevel, string> _logLevelStrings = new Dictionary<LogLevel, string>();
        private Dictionary<int, int> _threadIDs = new Dictionary<int, int>();

        private List<string> _logs = new List<string>();
        private string _filename, _levelString;
        private bool _levelStringSet;

        public LogLevel LogLevel { get; set; }

        public string LogFilePath { get; set; }

        public LightLogger(LogLevel level = LogLevel.DEBUG_LEVEL)
        {
            _filename = $"ContentManagerMigration_{DateTime.Now.ToString("yyyy-MM-dd")}.log";

            _logLevelStrings.Add(LogLevel.DEBUG_LEVEL, " DEBUG");
            _logLevelStrings.Add(LogLevel.ERROR_LEVEL, " ERROR");
            _logLevelStrings.Add(LogLevel.INFO_LEVEL, " INFO");
            _logLevelStrings.Add(LogLevel.WARN_LEVEL, " WARN");

            LogLevel = level;

            Log("    ======== Started Logging ========    ");
        }

        public string GetLogs()
        {
            var ret = string.Join(Environment.NewLine, _logs);
            _logs.Clear();

            return ret;
        }

        public void Log()
        {
            LogParams(null, LogLevel.DEFAULT);
        }

        public void Log(string message, bool logMethodName = true, LogLevel level = LogLevel.DEFAULT, params object[] parameters)
        {
            _levelString = _logLevelStrings.ContainsKey(level) ? _logLevelStrings[level] : _logLevelStrings[LogLevel];
            _levelStringSet = true;
			
            LogParams(message, level, parameters);
        }

        public void LogParams(string msg = null, params object[] ps)
        {
			LogParams(msg, LogLevel.DEFAULT, ps);
        }

        public void LogParams(string msg = null, LogLevel level = LogLevel.DEFAULT, params object[] ps)
        {
            _levelString = _levelStringSet ? _levelString : _logLevelStrings[LogLevel];

            msg = (string.Empty == (msg ?? string.Empty)) ? GetCaller(ps) : string.Format("{0} - {1}", GetCaller(ps), msg);
            Add(GetStamp() + msg, level);

            _levelStringSet = false;
        }

        public void LogException(object exception)
        {
            if (exception is Exception e)
            {
                AddRange(ExceptionHelper.GetExceptionMessages(e).Select(em => GetStamp() + em));
                Add(e.StackTrace, LogLevel.ERROR_LEVEL);
            }
        }

        private int GetThreadID()
        {
            var id = System.Threading.Thread.CurrentThread.ManagedThreadId;

            if (!_threadIDs.ContainsKey(id))
                _threadIDs.Add(id, _threadIDs.Count + 1);

            return _threadIDs[id];
        }

        private void DumpLog()
        {
            if (!string.IsNullOrWhiteSpace(_filename))
            {
                var path = RelativePathFinder.GetPath(string.IsNullOrWhiteSpace(LogFilePath) ? "testlogs" : LogFilePath);

                if (!string.IsNullOrWhiteSpace(path) && Directory.Exists(path))
                {
                    var originalPath = path;
                    path = Path.Combine(originalPath, _filename ?? "none.log");
                    var failPath = Path.Combine(originalPath, "lightlogger.log");

                    if (!string.IsNullOrWhiteSpace(path))
                    {
                        try
                        {
                            using (var sw = File.AppendText(path))
                            {
                                _logs.ForEach(line => sw.WriteLine(line));
                                sw.Flush();

                                _logs.Clear();
                            }
                        }
                        catch (Exception ex)
                        {
                            LogException(ex);

                            try
                            {
                                using (var sw = File.AppendText(failPath))
                                {
                                    _logs.ForEach(line => sw.WriteLine(line));
                                    sw.Flush();

                                    _logs.Clear();
                                }
                            }
                            catch (Exception) { }
                        }
                    }
                }
            }
        }

        private void Add(string msg, LogLevel level = LogLevel.DEFAULT)
        {
            level = (level == LogLevel.DEFAULT) ? LogLevel : level;
            Trace.WriteLine(msg);
            Console.WriteLine(msg);
            if (level <= LogLevel)
            {
                _logs.Add(msg);
            }
            DumpLog();
        }

        private void AddRange(IEnumerable<string> sts, LogLevel level = LogLevel.DEFAULT)
        {
            level = (level == LogLevel.DEFAULT) ? LogLevel : level;
            sts.ToList().ForEach(l => Trace.WriteLine(l));
            if (level <= LogLevel)
            {
                _logs.AddRange(sts);
            }
            DumpLog();
        }

        private string GetStamp()
        {
            return $"{System.DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss.fff", System.Globalization.CultureInfo.InvariantCulture)} [{GetThreadID()}]{_levelString ?? string.Empty}{" LightLogger - "}";
        }

        private string GetCaller(object[] parameterValues)
        {
            StackFrame frame;
            var index = 3;

            do frame = new StackTrace(index++, false).GetFrame(0);
            while ((frame?.GetMethod().DeclaringType?.Name.Contains("c__DisplayClass") ?? false) ||
                   (frame?.GetMethod().Name.Contains(@"<") ?? false) ||
                   (frame?.GetMethod().DeclaringType?.Name.Contains(@"<>c") ?? false) ||
                   (frame?.GetMethod().DeclaringType?.Name.Contains("List`1") ?? false));

            var parameterString = GetParameterString(frame, parameterValues);

            return string.Format("{0}.{1}{2}", frame?.GetMethod()?.DeclaringType?.Name ?? "{unknown}", frame?.GetMethod()?.Name ?? "{unknown}", parameterString);
        }

        private string GetParameterString(StackFrame frame, object[] parameterValues)
        {
            var parameters = frame?.GetMethod()?.GetParameters();
            var values = parameterValues?.Select(v => v is string ? string.Format("\"{0}\"", v?.ToString() ?? "{null}") : v?.ToString() ?? "{null}").ToList() ?? new List<string>();
            var nameValueList = new List<string>();

            var list = (null != parameters)
                ? parameters.Select(p => p.Name).ToList()
                : new List<string>();

            if (list?.Count == values?.Count)
            {
                for (int i = 0; i < list.Count; i++)
                    nameValueList.Add(string.Format("{0} == {1}", list[i], values[i]));
            }
            else
            {
                values?.ForEach(v => nameValueList.Add(v));
            }

            return string.Format("({0})", string.Join(", ", nameValueList));
        }
    }

    public static class ExceptionHelper
    {
        public static string GetFormattedMessage(Exception e, bool showStack = false)
        {
            return showStack
                ? string.Join(string.Format("{0}{0}", Environment.NewLine), GetExceptionMessages(e)) + Environment.NewLine + e.StackTrace
                : string.Join(string.Format("{0}{0}", Environment.NewLine), GetExceptionMessages(e));
        }

        public static List<string> GetExceptionMessages(Exception e, List<string> list = null)
        {
            if (null == list)
                list = new List<string>();

            list.Add(e.Message);
            list.Add(e.StackTrace);

            if (e.InnerException != null)
                return GetExceptionMessages(e.InnerException, list);

            return list;
        }
    }

    public class RelativePathFinder
    {
        public static string GetPath(string folderName)
        {
            var dir = AssemblyDirectory;
            Directory.SetCurrentDirectory(dir);

            while (!Directory.Exists(folderName) && !File.Exists(Path.Combine(dir, folderName)) && dir.Length > 3)
            {
                if (Directory.Exists(@".."))
                {
                    Directory.SetCurrentDirectory(@"..");
                    dir = Directory.GetCurrentDirectory();
                }

                else
                {
                    Directory.SetCurrentDirectory(AssemblyDirectory);
                    return string.Empty;
                }
            }

            var returnValue = (dir.Length <= 3) ? string.Empty : Path.Combine(dir, folderName);

            Directory.SetCurrentDirectory(AssemblyDirectory);

            return returnValue;
        }

        public static string AssemblyDirectory
        {
            get
            {
                var codeBase = Assembly.GetEntryAssembly().CodeBase;
                var uri = new UriBuilder(codeBase);
                var path = Uri.UnescapeDataString(uri.Path);
                return Path.GetDirectoryName(path);
            }
        }
    }
}