using DTS.Base;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Data;
using System.Data.SqlClient;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using Microsoft.Win32;
using System.Windows;

namespace CMDosDumper
{
    internal class TilePalette
    {
        public TilePalette() { PixelColors = new List<ushort>(); }
        public ushort TileIndex;
        public ushort Index;
        public ushort PaletteIndex;
        public bool Flipped;
        public List<ushort> PixelColors;
    }

    internal class SixteenBySixteenTile
    {
        public SixteenBySixteenTile() 
        { 
            Values = new List<uint>();
            Decoded = new List<byte>();

            for (int i = 0; i < 16 * 16; i++)
                Values.Add(0);

            for (int i = 0; i < 16 * 2; i++)
                Decoded.Add(0);
        }

        private List<uint> Values;
        private List<byte> Decoded;

        public List<uint> GetUniqueValues() { return Values.Distinct().OrderBy(v => v).ToList(); }

        public void AddNybble(int rowIndex, int pxOffset, byte val, byte startMask, uint contribution)
        {
            var index = (rowIndex % 16) * 16;
            var pxchange = 0x3;
            for (uint i = startMask; i < startMask*0x10; i*=2, pxchange--)
            {
                if ((val & i) > 0)
                {
                    if (Values.Count <= index + pxOffset + pxchange)
                    {
                        Values[index + pxOffset + pxchange] = contribution;
                    }

                    else Values[index + pxOffset + pxchange] |= contribution;
                }
            }
        }

        public void SetNybble(int rowIndex, int pxOffset, byte val, byte startMask)
        {
            var index = rowIndex * 2;
            byte bitToSet = (byte)((pxOffset % 8) > 0 ? 0x1 : 0x10);
            var destIndex = (index % 0x20) + (rowIndex >= 16 ? 1 : 0);

            for (byte i = startMask; i < startMask * 0x10 && i != (byte)0x0; i *= 2, bitToSet *=2)
            {
                if ((val & i) > 0)
                {
                    Decoded[destIndex] |= bitToSet;
                }
            }
        }

        public void SetByte(int rowIndex, byte srcByte1, byte srcByte2, byte startMask)
        {
            var destIndex = (rowIndex % 0x10) * 2 + (rowIndex >= 0x10 ? 1 : 0);

            byte bitToSet = 0x10;
            for (byte i = startMask; i < startMask * 0x10 && i != (byte)0x0; i *= 2, bitToSet *= 2)
            {
                if ((srcByte1 & i) > 0)
                {
                    Decoded[destIndex] |= bitToSet;
                }
            }

            bitToSet = 0x1;
            for (byte i = startMask; i < startMask * 0x10 && i != (byte)0x0; i *= 2, bitToSet *= 2)
            {
                if ((srcByte2 & i) > 0)
                {
                    Decoded[destIndex] |= bitToSet;
                }
            }
        }

        public uint Read(uint x, uint y)
        {
            return Values[(int)(y * 16 + x)];
        }

        public uint Read(int index)
        {
            return Values[index];
        }

        public override string ToString()
        {
            var list = new List<string>();
            for(int y=0; y < 16; y++)
            {
                var row = string.Empty;
                for(int x = 0; x < 16; x++)
                {
                    row += $"{Values[x + y*16]:X1}";
                } 

                list.Add(row);
            }
            return string.Join(Environment.NewLine, list);
        }

        public string convertBytes(string character)
        {
            var list = new List<string>();
            for (int y = 0; y < 16; y++)
            {
                var row = string.Empty;
                for (int x = 0; x < 16; x++)
                {
                    row += $"{Values[x + y * 16]:X1}".Replace(character, "1");
                }

                list.Add(hexBytesFromRow(row));
            }
            return string.Join(Environment.NewLine, list);
        }

        public string Encoded()
        {
            var rows = new List<string>();
            for(int i = 0; i < 2; i++)
            {
                var list = new List<string>();
                for (int j = 0; j < 16; j++)
                    list.Add($"{Decoded[i * 16 + j]:X2}");

                rows.Add(string.Join(" ", list));
            }

            return string.Join(Environment.NewLine, rows);
        }

        public string hexBytesFromRow(string row)
        {
            var listOfHex = new List<string>();

            if (null == row || (row.Length % 8 > 0))
                return "SORRY";

            var chars = row.ToList();
            for(int i = 0; i < row.Length / 8; i++)
            {
                var shorterList = chars.Skip(i * 8).Take(8).ToList();
                byte hexval = 0;
                for(byte k = 0x80,j=0; k > 0; k /= 2,j++)
                {
                    if (shorterList[j] == '1')
                        hexval |= k;
                }

                listOfHex.Add($"{hexval:X2}");
            }

            return string.Join(" ", listOfHex);
        }
    }

    internal class CBMSector
    {
        internal CBMSector(int sectorNum)
        {
            Sector = sectorNum;
            Bytes = new List<byte>();
        }

        internal int Sector { get; private set; }
        internal List<byte> Bytes { get; private set; }

        internal void Add(byte[] bytes)
        {
            if (null != bytes)
            {
                Bytes.Clear();
                var list = bytes.Length > 256 ? bytes.Take(256).ToList() : bytes.ToList();

                Bytes.AddRange(list);
            }
        }

        internal string GetDecimalBytes()
        {
            var list = Bytes.Count == 256 ? Bytes : Bytes.Concat(Get256Zeroes()).Take(256).ToList();
            return string.Join(", ", list.Select(b => ((int)b).ToString()));
        }

        internal string GetHexBytes()
        {
            var list = Bytes.Count == 256 ? Bytes : Bytes.Concat(Get256Zeroes()).Take(256).ToList();
            return string.Join(" ", list.Select(b => (b).ToString("x2"))).ToUpper();
        }

        internal string GetASCII()
        {
            var list = Bytes.Count == 256 ? Bytes : Bytes.Concat(Get256Zeroes()).Take(256).ToList();
            return string.Join("", list.Select(b => !Printable(b) ? "." : Encoding.ASCII.GetString(new byte[] { b })));
        }

        internal int LoadBytes(byte[] bytes, int offset)
        {
            var gtg = bytes.Length >= offset + 256;

            if (gtg)
            {
                Bytes.Clear();
                Bytes.AddRange(bytes.Skip(offset).Take(256));
            }

            return gtg ? offset + 256 : offset;
        }

        private bool Printable(byte b)
        {
            var i = (int)b;
            return i >= 32 && ((i < 126) || (i == 127) || ((i > 144) && (i < 149)) || (i == 150) || (i == 151));
        }

        private List<byte> Get256Zeroes()
        {
            var list = new List<byte>();
            for (int i = 0; i < 256; i++)
                list.Add((byte)0);

            return list;
        }
    }

    internal class CBMTrack
    {
        internal CBMTrack(int trackNum)
        {
            Track = Math.Min(35, Math.Max(1, trackNum));

            Sectors = new List<CBMSector>();

            int maxSectors = Track < 18 ? 21 : (Track < 25 ? 19 : (Track < 31 ? 18 : 17));
            for(int i=0; i < maxSectors; i++)
            {
                Sectors.Add(new CBMSector(i));
            }
        }

        internal List<CBMSector> Sectors { get; private set; }

        internal int Length()
        {
            return (Sectors?.Count ?? 0) * 256;
        }

        internal string GetHex(int offset)
        {
            StringBuilder sb = new StringBuilder();

            foreach(var sector in Sectors)
            {
                sb.Append($"0x{offset:X8}\t");
                sb.Append(string.Format("({0:d2},{1:d2}):\t", Track, sector.Sector));
                sb.AppendLine(sector.GetHexBytes());

                offset += 256;
            }

            return sb.ToString();
        }

        internal string GetASCII(int offset)
        {
            StringBuilder sb = new StringBuilder();

            foreach (var sector in Sectors)
            {
                sb.Append($"0x{offset:X8}\t");
                sb.Append(string.Format("({0:d2},{1:d2}):\t", Track, sector.Sector));
                sb.AppendLine(sector.GetASCII());

                offset += 256;
            }

            return sb.ToString();
        }

        internal string GetDecimal(int offset)
        {
            StringBuilder sb = new StringBuilder();

            foreach (var sector in Sectors)
            {
                sb.Append($"{offset:d8}\t");
                sb.Append(string.Format("({0:d2},{1:d2}):\t", Track, sector.Sector));
                sb.AppendLine(sector.GetDecimalBytes());

                offset += 256;
            }

            return sb.ToString();
        }

        internal int LoadBytes(byte[] bytes, int offset)
        {
            for (int i = 0; i < Sectors.Count; i++)
            {
                offset = Sectors[i].LoadBytes(bytes, offset);
            }

            return offset;
        }

        internal int Track { get; private set; }
    }

    internal class CBMDisk
    {
        internal CBMDisk(string volumeName)
        {
            Name = null == volumeName ? "GENERIC VOLUME" : volumeName;

            Tracks = new List<CBMTrack>();

            for(int i=1; i<36; i++)
            {
                Tracks.Add(new CBMTrack(i));
            }
        }

        internal List<CBMTrack> Tracks { get; private set; }

        internal string GetHex()
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendLine("Disk: \"" + Name + "\"");
            sb.AppendLine();

            var offset = 0;
            foreach (var track in Tracks)
            {
                sb.Append(track.GetHex(offset));
                offset += track.Length();
            }

            return sb.ToString();
        }

        internal string GetASCII()
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendLine("Disk: \"" + Name + "\"");
            sb.AppendLine();

            var offset = 0;
            foreach (var track in Tracks)
            {
                sb.Append(track.GetASCII(offset));
                offset += track.Length();
            }

            return sb.ToString();
        }

        internal string GetDecimal()
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendLine("Disk: \"" + Name + "\"");
            sb.AppendLine();

            var offset = 0;
            foreach (var track in Tracks)
            {
                sb.Append(track.GetDecimal(offset));
                offset += track.Length();
            }

            return sb.ToString();
        }

        internal void Open(string filename)
        {
            var bytes = File.ReadAllBytes(filename);
            var offset = 0;

            for(int i=0; i < 35; i++)
            {
                offset = Tracks[i].LoadBytes(bytes, offset);
            }
        }

        internal string Name { get; private set; }
    }

    internal class CBMFile
    {
        public string Name;
        public string Extension;
        public int blocksUsed;
        public byte startingTrack;
        public byte startingSector;
        public List<byte> filenameBytes;
    }

    public class TS
    {
        public int track;
        public int sector;
        public int offset;
    }

    public class RoomCode
    {
        public int a;
        public int b;
        public int c;
    }

    public class Room
    {
        public Room()
        {
            tracksAndSectors = new Dictionary<int, TS>();
            trackAndSectorBases = new List<int>();
        }

        public Dictionary<int, TS> tracksAndSectors;
        public List<int> trackAndSectorBases;

        public string Name;
        public string Desc;
        public int x;
        public int y;
        public int z;
        public int startingTrack { get { return tracksAndSectors.Count > 0 ? tracksAndSectors.First().Value.track : 0; } }
        public int startingSector { get { return tracksAndSectors.Count > 0 ? tracksAndSectors.First().Value.sector : 0; } }
        public int a;
        public int b;
        public int c;
        public int initSectorOffset;
        public int baseAddress;

        public void Add(int address, TS ts)
        {
            if (tracksAndSectors.ContainsKey(address))
                tracksAndSectors[address] = ts;

            else tracksAndSectors.Add(address, ts);
        }

        public void Add(int address)
        {
            if (!trackAndSectorBases.Contains(address))
                trackAndSectorBases.Add(address);
        }

        public TS GetByAddress(int address)
        {
            return tracksAndSectors.ContainsKey(address) ? tracksAndSectors[address] : new TS();
        }

        public TS Get(int index)
        {
            return tracksAndSectors.Count > index && index >= 0 ? tracksAndSectors.Select(ts => ts.Value).ToList()[index] : new TS();
        }

        public int GetAddress(int index)
        {
            return trackAndSectorBases.Count > index && index >= 0 ? trackAndSectorBases[index] : 0;
        }

        public int GetAddress(TS ts)
        {
            return tracksAndSectors.Where(p => p.Value == ts).Select(p => p.Key).FirstOrDefault();
        }
    }

    partial class Program
    {
        static Dictionary<byte, string> commandList = new Dictionary<byte, string>();

        private static bool Printable(byte b)
        {
            var i = (int)b;
            return i >= 32 && ((i < 126) || (i == 127) || ((i > 144) && (i < 149)) || (i == 150) || (i == 151));
        }

        private static List<string> GetUpDown(byte b)
        {
            var list = new List<string>();

            if ((b & 1) == 1)
                list.Add("UP");

            if ((b & 2) == 2)
                list.Add("DOWN");

            

            return list;
        }

        private static string GetRoomName(byte[] bytes)
        {
            StringBuilder sb = new StringBuilder();

            var p = 0;

            while(p < bytes.Length)
            {
                var i = (int)bytes[p];
                if (i == 13 || i == 42)
                    break;

                byte b = (byte)((int)(bytes[p++]) & 127);

                if (i < 32)
                    continue;

                sb.Append($"{Encoding.ASCII.GetString(new byte[] { b })}");
            }

            return sb.ToString();
        }

        private static string GetDescription(byte[] bytes)
        {
            var str = string.Empty;

            var p = 0;

            var returnPrinted = false;

            while (p < bytes.Length)
            {
                var i = (int)bytes[p];
                byte b = ((i < 32) ? (byte)32 : (byte)(i & 127));
                var s = $"[{i}]";
                var e = $"{Encoding.ASCII.GetString(new byte[] { b })}";
                e = i > 64 && i < 91 ? e.ToLower() : e;
                str += returnPrinted ? string.Empty : (i == 42 ? "\n" : string.Empty);
                str += i == 13 ? "[n]\n" : (i < 32 ? s : e);

                returnPrinted = i == 13;
                p++;
            }

            return str;
        }

        private static int SearchForDescriptor(int p, int [] descs, byte[] bytes, ref bool previousDescriptorWasFound, ref int i)
        {
            var init = p;

            while (i < descs.Length)
            {
                var descriptor = descs[i];

                while (p < bytes.Length)
                {
                    if ((int)bytes[p] == 42 && (int)bytes[p+1] == descriptor)
                        return p+1;

                    if (previousDescriptorWasFound && (int)bytes[p + 1] < 13)   // found another descriptor;
                    {
                        previousDescriptorWasFound = false;
                        break;
                    }

                    p++;
                }

                i++;
                p = init;
            }

            return -1;
        }

        private static void GetCodes(Room room, string name, Dictionary<string, List<RoomCode>> codeList, byte[] bytes)
        {
            var p = 0;
            var index = 0;
            var adjustment = 0;

            var address = room.tracksAndSectors.Keys.FirstOrDefault();
            var ts = room.tracksAndSectors[address];

            var coordName = $"({room.x}, {room.y}, {room.z})";
            var allCodes = codeList.ContainsKey(coordName) ? codeList[coordName] : new List<RoomCode>();

            var allCodesString = "CODE COMBINATIONS: " + string.Join(" ", allCodes.Select(c => $"[{c.a},{c.b},{c.c}]"));

            WriteLine($"\n{name}: " + allCodesString);

            while (p < bytes.Length)
            {
                try
                {
                    if (room.trackAndSectorBases.Contains(p))
                    {
                        ts = room.Get(Math.Max(0, room.trackAndSectorBases.IndexOf(p) + 1));
                        address = room.GetAddress(ts);
                        adjustment = p;
                    }

                    var first = (int)bytes[p];

                    if (first == 42)
                    {
                        var next = (int)bytes[p + 1];

                        var fullOffset = address + p + ts.offset + 1 - adjustment;
                        var full = $" (Full Disk offset on T/S ({ts.track},{ts.sector}) ${fullOffset:X5} ({fullOffset:d}))";

                        if (next < 13)
                            WriteLine($"Code #{++index:d} [{next:d}] at offset ${(p + 1):X3} ({(p + 1):d}) " + full);
                        
                        else if(next > 31 || next == 13)
                            WriteLine($"NOTE: Character ${next:X2} ({next:d}) at offset ${(p + 1):X3} ({(p + 1):d}) " + full);
                    }

                    p++;
                }
                catch (Exception ex)
                {

                }
            }
        }

        private static string GetFullDescription(List<string> errors, byte[] bytes, int d1, int d2, int d3)
        {
            var str = string.Empty;

            var p = 0;

            var returnPrinted = false;

            var f = 0;
            var flags = new List<int>() { d1, d2, d3 };
            var col = 0;

            var i = 0;
            var descriptor = -1;
            var found = false;

            var breakIfNoMoreDescriptors = false;

            while (p < bytes.Length)
            {
                try
                {
                    i = (int)bytes[p];
                    if (i == 42)
                    {
                        if ((int)bytes[p + 1] < 13)
                        {
                            var previousDescriptorWasFound = found;

                            var pointer = SearchForDescriptor(p, flags.ToArray(), bytes, ref previousDescriptorWasFound, ref f);
                            found = pointer != -1;
                            p = !found ? p + 1 : pointer;

                            descriptor = flags[Math.Min(2, f)];

                            if (f == 3 && !found)
                                breakIfNoMoreDescriptors = true;

                            if (!found)
                                errors.Add($"Couldn't find descriptor {descriptor} (position #{Math.Min(2, f)})... skipping...");

                            if (p >= bytes.Length)
                                break;

                            continue;
                        }
                        else i = (int)bytes[p++];
                    }

                    if (breakIfNoMoreDescriptors && i < 32 && i != 13)
                        break;
                    
                    var val = i < 32 ? 0 : 1;
                    col = returnPrinted ? val : col + val;
                    var extra = col == 40 ? "\n" : string.Empty;

                    byte b = ((i < 32) ? (byte)32 : (byte)(i & 127));
                    var s = i == 13 ? "\n" : string.Empty;
                    var e = $"{Encoding.ASCII.GetString(new byte[] { b })}";
                    e = i > 64 && i < 91 ? e.ToLower() : e;
                    str += i < 32 ? s : e;
                    str += extra;

                    returnPrinted = i == 13 || extra == "\n";
                    p++;
                }
                catch (Exception ex) 
                { 
                
                }
            }

            return str;
        }

        private static List<string> GetDiskDescriptionsInBlock(byte[] bytes)
        {
            var list = new List<string>();

            var p = 0;
            var f = 0;
            var col = 0;

            var returnPrinted = false;

            var i = 0;
            string str = null;
            var initial = string.Empty;
            var link = string.Empty;
            var found = false;

            while (p < bytes.Length - 1)
            {
                try
                {
                    i = (int)bytes[p];
                    //if (i == 42 || p == 0 || i == 123)
                    //{
                    if (i < 13)
                    //if ((i == 42 && (int)bytes[p + 1] < 13) || (p == 0 && i < 13))
                    {
                        //if (null != str && string.Empty != initial)
                        //{
                        //    if (initial?.Replace("\n\n", "\n") == "\n")
                        //        str = string.Join("\n", str.Split("\n").Where(s => s.Length > 0)) + "[RETURN]";

                        //    list.Add(str + "\"" + link);
                        //}

                        //if (i == 42)
                        //    p++;

                        str = $"[{((int)(bytes[p])):d}]\n\"";
                        initial = string.Empty;
                        link = string.Empty;
                        found = true;
                        //p++;

                        //if (p >= bytes.Length)
                        //    break;

                        //continue;
                    }
                    else
                    {
                        found = false;
                        p++;

                        //while (p < bytes.Length)
                        //{
                        byte by = bytes[p];

                        //if ((int)by == 42)
                        //    break;

                        if (i == 123 && string.Empty != initial)
                        {
                            if (p + 1 < bytes.Length)
                            {
                                link = $" => ({((int)(bytes[p])):d},{((int)(bytes[p + 1])):d})";
                            }
                        }

                        // p++;
                        //}

                        continue;
                    }

                    //}

                    while (i != 42)
                    {
                        var val = i < 32 ? 0 : 1;
                        col = returnPrinted ? val : col + val;
                        var extra = col == 40 ? "\n" : string.Empty;

                        byte b = ((i < 32) ? (byte)32 : (byte)(i & 127));
                        var s = i == 13 ? "\n" : string.Empty;
                        var e = $"{Encoding.ASCII.GetString(new byte[] { b })}";
                        e = i > 64 && i < 91 ? e.ToLower() : e;
                        str += i < 32 ? s : e;
                        str += extra;

                        initial += i < 32 ? s : e;
                        initial += extra;

                        returnPrinted = i == 13 || extra == "\n";

                        p++;

                        if (p >= bytes.Length)
                            break;

                        i = (int)bytes[p];
                    }

                    if (null != str && string.Empty != initial)
                    {
                        if (initial?.Replace("\n\n", "\n") == "\n")
                            str = string.Join("\n", str.Split("\n").Where(s => s.Length > 0)) + "[RETURN]";

                        list.Add(str + "\"" + link);
                    }
                }
                catch (Exception ex)
                {

                }

                p++;
            }

            if (null != str && str != string.Empty && !list.Contains(str) && string.Empty != initial)//&& found 
            {
                if (!list.Contains(str + "\"" + link))
                    list.Add(str + "\"" + link);

                link = string.Empty;
            }

            return list;
        }

        private static List<string> GetDirection(byte b)
        {
            var list = new List<string>();

            if ((b & 1) == 1)
                list.Add("N");

            if ((b & 2) == 2)
                list.Add("S");

            if ((b & 4) == 4)
                list.Add("W");

            if ((b & 8) == 8)
                list.Add("E");

            if ((b & 16) == 16)
                list.Add("NW");

            if ((b & 32) == 32)
                list.Add("NE");

            if ((b & 64) == 64)
                list.Add("SW");

            if ((b & 128) == 128)
                list.Add("SE");

            return list;
        }

        private static int GetBaseAddress(byte t, byte s)
        {
            var setOne = 21 * 256;
            var setTwo = 19 * 256;
            var setThree = 18 * 256;
            var setFour = 17 * 256;

            var trackSize =   t > 30 ? setFour
                           : (t > 24 ? setThree
                           : (t > 17 ? setTwo : setOne));

            var offset = t > 30 ? ((17 * setOne) + (8 * setTwo) + (7 * setThree)) 
                      : (t > 24 ? ((17 * setOne) + (8 * setTwo)) 
                      : (t > 17 ?  (17 * setOne) : 0));

            var index = t > 30 ? t - 31
                           : (t > 24 ? t - 25
                           : (t > 17 ? t - 18 : t - 1));

            var baseAddress = (index * trackSize) + offset;

            return baseAddress + (int)s * 256;
        }

        private static int GetOffsetInBlock(byte[] block, byte x, byte y, byte z)
        {
            var offset = -1;
            var p = 0;

            while ((p + 2) < 256)
            {
                if (block[p] == x && block[p + 1] == y && block[p + 2] == z)
                {
                    offset = p + 3;
                    p = 255;
                }

                p++;
            }

            return offset;
        }

        private static void CopyUntilAsterisk(List<byte> destination, byte[] block)
        {
            var i = 0;
            while((i+1) < block.Length)
            {
                byte b = block[i];

                if ((int)b == 42 && (int)block[i+1] == 123)
                    break;

                destination.Add(b);

                i++;
            }
        }

        private static void FixUp(Room room, List<byte> destination)
        {
            var list = new List<int>();
            foreach(var offset in room.trackAndSectorBases)
            {
                var added = false;
                var orig = offset;
                var i = 0;
                while(destination.Count > i)
                {
                    if((int)destination[i] == 42 && (int)destination[i + 1] < 13)
                    {
                        list.Add(i);
                        added = true;
                        break;
                    }

                    i++;
                }

                if (!added)
                    list.Add(orig);
            }

            room.trackAndSectorBases.Clear();
            room.trackAndSectorBases.AddRange(list);
        }

        private static void CopyAllDataAcrossSectors(Room room, List<byte> destination, byte[] filebytes, int baseAddress, int blockSize, byte x, byte y, byte z)
        {
            int p = 0;
            while ((p + 3) < blockSize)
            {
                if ((int)filebytes[baseAddress + p] == 42 && (int)filebytes[baseAddress + p + 1] == 123)
                {
                    if ((int)filebytes[baseAddress + p + 2] != 42)
                    {
                        var t = (int)filebytes[baseAddress + p + 2];
                        var s = (int)filebytes[baseAddress + p + 3];

                        baseAddress = GetBaseAddress(filebytes[baseAddress + p + 2], filebytes[baseAddress + p + 3]);
                        var offset = GetOffsetInBlock(filebytes.Skip(baseAddress).Take(256).ToArray(), x, y, z);

                        var ts = new TS() { track = t, sector = s, offset = offset };
                        room.Add(baseAddress, ts);
                        room.Add(destination.Count);

                        CopyAllDataAcrossSectors(room, destination, filebytes, baseAddress + offset, 256 - offset, x, y, z);
                    }

                    p = blockSize;
                }

                else destination.Add(filebytes[baseAddress + p++]);
            }
        }

        private static int GetInitialTrackOffset(byte[] filebytes, int baseAddress, byte x, byte y, byte z)
        {
            var arr = filebytes.Skip(baseAddress).Take(256).ToArray();
            return GetOffsetInBlock(arr, x, y, z);
        }

        private static void GetRoomDescription(List<byte> destination, byte[] filebytes, int baseAddress, byte x, byte y, byte z)
        {
            try
            {
                var arr = filebytes.Skip(baseAddress).Take(256).ToArray();
                var offset = GetOffsetInBlock(arr, x, y, z);
                if(-1 != offset)
                {
                    CopyUntilAsterisk(destination, arr.Skip(offset).ToArray());
                }
            }
            catch (Exception ex) 
            {  
            }
        }

        private static void GetRoomData(Room room, List<byte> destination, byte[] filebytes)
        {
            try
            {
                if (-1 != room.initSectorOffset)
                {
                    CopyAllDataAcrossSectors(room, destination, filebytes, room.baseAddress + room.initSectorOffset, 256 - room.initSectorOffset, (byte)room.x, (byte)room.y, (byte)room.z);
                    //FixUp(room, destination);
                }
            }
            catch (Exception ex) 
            { 
            }
        }

        [STAThread]
        static void Main(string[] args)
        {
            //GetScreenText();
            //GetScreenText();

            //DoNodes(args);
            //GetDiskMessages();
            //DoRoomDescs(args);
            //PackDiskWithProgram();

            //Encode();
            //ExtractPrograms(); // EXTRACTS C64 programs from virtual disk (latest)

            //PackDisk();
            //EncodeDisk();

            //DecodeTileData(); // FOR BLACK TIGER PROJECT
            //ConvertBlackTigerBitmap(); // FOR BLACK TIGER PROJECT
            //InterleaveBitplanes();// FOR BLACK TIGER PROJECT

            //GetLevelMapsPixelColors();// FOR BLACK TIGER PROJECT
            //FixTileIndexes();// FOR BLACK TIGER PROJECT
            FixTileIndexes2();// FOR BLACK TIGER PROJECT
        }

        static void FixTileIndexes2()
        {
            var bytes = new List<byte>();
            var output = new List<byte>();
            var dir = Directory.GetCurrentDirectory() ?? string.Empty;
            var list = new List<string>();

            FileIO.ForEachFileIn(dir, file =>
            {
                var filename = Path.GetFileName(file);

                if (filename.ToLower().StartsWith("indicies.bin"))
                {
                    if (!list.Contains(file))
                        list.Add(file);
                }
            });

            foreach (var file in list)
            {
                bytes = File.ReadAllBytes(file).ToList();

                var outIndexes = new List<string>();
                for(int i = 0; i < bytes.Count; i+=2)
                {
                    byte index = (byte)((int)bytes[i] - 1);
                    byte firstByte = (bytes[i + 1] > 0) ? (byte)8 : (byte)0;

                    output.Add(firstByte);
                    output.Add(index);
                }
            }

            File.WriteAllBytes("Level01.map", output.ToArray());

            WriteLine("Press any key to end");
            Console.ReadKey();
        }


        static void FixTileIndexes()
        {
            var lines = new List<string>();
            var output = new List<string>();
            var dir = Directory.GetCurrentDirectory() ?? string.Empty;
            var list = new List<string>();

            FileIO.ForEachFileIn(dir, file =>
            {
                var filename = Path.GetFileName(file);

                if (filename.ToLower().StartsWith("poison.txm"))
                {
                    if (!list.Contains(file))
                        list.Add(file);
                }
            });

            foreach (var file in list)
            {
                lines = File.ReadAllLines(file).ToList();

                var outIndexes = new List<string>();
                foreach(var line in lines)
                {
                    var indicies = line.Split(",").ToList();
                    foreach(var index in indicies)
                    {
                        int val = -1;
                        if(int.TryParse(index, out val))
                        {
                            if (0 == val || 1 == val)
                                val = 0;

                            else val = val + 0xfb;
                            outIndexes.Add($"{val:X3}");
                        }
                    }

                    output.Add(string.Join(" ", outIndexes));
                }
            }

            foreach(var line in output)
            {
                WriteLine(line);
            }

            File.WriteAllLines("BetterLevel01.txt", output);

            WriteLine("Press any key to end");
            Console.ReadKey();
        }

        static void GetLevelMapsPixelColors()
        {
            var bytes = new List<byte>();
            ushort[][] mapPaletteValues = new ushort[16][];

            mapPaletteValues[0] = new ushort[] { 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0110 };
            mapPaletteValues[1] = new ushort[] { 0x0b87, 0x0433, 0x0842, 0x0a53, 0x0c64, 0x0db8, 0x0974, 0x0754, 0x0644, 0x0c95, 0x0f85, 0x0ffa, 0x0cca, 0x0998, 0x0666, 0x0111 };
            mapPaletteValues[2] = new ushort[] { 0x0b87, 0x0544, 0x0754, 0x0975, 0x0ca8, 0x0eea, 0x0fc4, 0x0653, 0x0974, 0x0c84, 0x0eef, 0x0aaa, 0x0889, 0x0778, 0x0556, 0x0111 };
            mapPaletteValues[3] = new ushort[] { 0x0b87, 0x0754, 0x0975, 0x0ca8, 0x0ed8, 0x0fff, 0x0060, 0x0090, 0x00e0, 0x0777, 0x0aaa, 0x0747, 0x0868, 0x0a8a, 0x0cac, 0x0111 };
            mapPaletteValues[4] = new ushort[] { 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0520, 0x0730, 0x0940, 0x0b50, 0x0d60, 0x0f80, 0x0000, 0x0111 };
            mapPaletteValues[5] = new ushort[] { 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0455, 0x0600, 0x0900, 0x0986, 0x0a97, 0x0000, 0x0000, 0x0000, 0x0000 };
            mapPaletteValues[6] = new ushort[] { 0x0046, 0x089c, 0x0789, 0x0678, 0x0567, 0x0456, 0x0345, 0x0540, 0x0753, 0x0864, 0x0a75, 0x0c86, 0x0ea7, 0x0fc8, 0x0ffa, 0x0000 };
            mapPaletteValues[7] = new ushort[] { 0x0000, 0x0afd, 0x07ec, 0x00c9, 0x00a7, 0x0086, 0x0064, 0x0050, 0x0040, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 };
            mapPaletteValues[8] = new ushort[] { 0x0000, 0x0fb9, 0x0e98, 0x0d86, 0x0c75, 0x0a64, 0x0853, 0x0640, 0x0435, 0x0857, 0x0b75, 0x0b5a, 0x089c, 0x0789, 0x0046, 0x0000 };
            mapPaletteValues[9] = new ushort[] { 0x0000, 0x0cb8, 0x0ba7, 0x0a96, 0x0985, 0x0874, 0x0763, 0x0650, 0x0540, 0x0430, 0x0dd0, 0x0d90, 0x0c70, 0x0900, 0x0700, 0x0000 };
            mapPaletteValues[10] = new ushort[] { 0x0000, 0x0540, 0x0750, 0x0940, 0x0e70, 0x0340, 0x0450, 0x0560, 0x0670, 0x0780, 0x0990, 0x0aa0, 0x0cc0, 0x0de0, 0x0ef0, 0x0000 };
            mapPaletteValues[11] = new ushort[] { 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0455, 0x0566, 0x0776, 0x0986, 0x0a97, 0x0000, 0x0000, 0x0000, 0x0000 };
            mapPaletteValues[12] = new ushort[] { 0x0000, 0x0899, 0x0aaa, 0x0998, 0x0887, 0x0776, 0x0665, 0x0554, 0x0440, 0x0000, 0x0000, 0x0000, 0x089c, 0x0789, 0x0046, 0x0000 };
            mapPaletteValues[13] = new ushort[] { 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 };
            mapPaletteValues[14] = new ushort[] { 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0035, 0x0146, 0x0257, 0x0368, 0x0479, 0x058a, 0x0abc, 0x0000 };
            mapPaletteValues[15] = new ushort[] { 0x0111, 0x0FF9, 0x0EC7, 0x0DA6, 0x0C85, 0x0A74, 0x0864, 0x0753, 0x0641, 0x0533, 0x0431, 0x0111, 0x0111, 0x0111, 0x0111, 0x0110 };

            var levelTilePalettes = new List<List<TilePalette>>();
            
            var dir = Directory.GetCurrentDirectory() ?? string.Empty;
            var list = new List<string>();

            FileIO.ForEachFileIn(dir, file =>
            {
                var filename = Path.GetFileName(file);

                if (filename.ToLower().StartsWith("tile_data_all_levels.bin"))
                {
                    if (!list.Contains(file))
                        list.Add(file);
                }
            });

            var bankSize = 0x4000;
            var bank = 9;
            var level = 1;
            var offset = 0;
            var blockOffset = 0;
            var rowBlockOffset = 0;
            var rowMultiplier = 0;

            foreach (var file in list)
            {
                bytes = File.ReadAllBytes(file).ToList();
                for (int i = level; i < 9; i++)
                {
                    var tilePaletteList = new List<TilePalette>();
                    int rowLimit = (level == 3) ? 128 : 64;
                    int colLimit = (level == 3) ? 64 : 128;

                    WriteLine("Level " + level++.ToString() + "; Bank " + bank++.ToString());
                    var levelBytes = bytes.Skip(offset).Take(bankSize).ToList();
                    offset += bankSize;

                    rowBlockOffset = 0;
                    for (int row = 0; row < rowLimit; row++)
                    {
                        rowMultiplier = row % 16;
                        if ((0 == rowMultiplier) && row > 0)
                        {
                            rowBlockOffset += ((level-1) == 3) ? 0x800 : 0x1000;
                        }

                        for (int col = 0; col < colLimit; col++)
                        {
                            if ((0 == col % 16) && col > 0)
                            {
                                blockOffset += 32 * 15;
                            }

                            var tilePalette = new TilePalette();
                            var byteA = levelBytes[(rowMultiplier * 32) + col * 2 + blockOffset + rowBlockOffset];                 //row * (colLimit * 2)
                            var byteB = levelBytes[(rowMultiplier * 32) + col * 2 + 1 + blockOffset + rowBlockOffset];             //row * (colLimit * 2)
                            var tileIndex = byteA + ((byteB & 0x7) << 8);
                            ushort paletteIndex = (ushort)((byteB & 0x78) >> 3);

                            tilePalette.PaletteIndex = paletteIndex;
                            tilePalette.TileIndex = (ushort)(tileIndex);
                            tilePalette.Index = (ushort)(row * colLimit + col);
                            tilePalette.Flipped = (byteB & 0x80) == 0x80;

                            tilePaletteList.Add(tilePalette);
                        }

                        blockOffset = 0;
                    }

                    levelTilePalettes.Add(tilePaletteList);
                }
            }

            dir = Directory.GetCurrentDirectory() ?? string.Empty;
            list = new List<string>();
            var output = new List<string>();

            FileIO.ForEachFileIn(dir, file =>
            {
                var filename = Path.GetFileName(file);

                if (filename.ToLower().StartsWith("gfx2.bin"))
                {
                    if (!list.Contains(file))
                        list.Add(file);
                }
            });

            var allTiles = new List<SixteenBySixteenTile>();
            bytes = new List<byte>();
            var sheetRows = 0x80;
            var sheetColumns = 16;

            var lowerBitplanesOffset = 0x20000;

            foreach (var file in list)
            {
                bytes = File.ReadAllBytes(file).ToList();
            }

            int sourceRowOffset = 0;
            for (int sheetRow = 0; sheetRow < sheetRows; sheetRow++)
            {
                for (int sheetCol = 0; sheetCol < sheetColumns; sheetCol++)
                {
                    var tile = new SixteenBySixteenTile();

                    // Now, we need to iterate over the rows and columns of BITS
                    for (int sourceRowIndex = 0; sourceRowIndex < 0x40; sourceRowIndex += 2)
                    {
                        var pxOffset = sourceRowIndex < 0x20 ? 0 : 0x8;
                        var zeroAndOneByte1 = bytes[lowerBitplanesOffset + sourceRowOffset + sourceRowIndex];
                        var zeroAndOneByte2 = bytes[lowerBitplanesOffset + sourceRowOffset + sourceRowIndex + 1];
                        var twoAndThreeByte1 = bytes[sourceRowIndex + sourceRowOffset];
                        var twoAndThreeByte2 = bytes[sourceRowIndex + sourceRowOffset + 1];

                        //Bitplane 01 - lower nybble
                        uint contribution = 8;
                        tile.AddNybble(sourceRowIndex / 2, 0 + pxOffset, zeroAndOneByte1, 0x1, contribution);
                        tile.AddNybble(sourceRowIndex / 2, 4 + pxOffset, zeroAndOneByte2, 0x1, contribution);

                        //Bitplane 00 - upper nybble
                        contribution /= 2;
                        tile.AddNybble(sourceRowIndex / 2, 0 + pxOffset, zeroAndOneByte1, 0x10, contribution);
                        tile.AddNybble(sourceRowIndex / 2, 4 + pxOffset, zeroAndOneByte2, 0x10, contribution);

                        //Bitplane 03 - lower nybble
                        contribution /= 2;
                        tile.AddNybble(sourceRowIndex / 2, 0 + pxOffset, twoAndThreeByte1, 0x1, contribution);
                        tile.AddNybble(sourceRowIndex / 2, 4 + pxOffset, twoAndThreeByte2, 0x1, contribution);

                        //Bitplane 02 - upper nybble
                        contribution /= 2;
                        tile.AddNybble(sourceRowIndex / 2, 0 + pxOffset, twoAndThreeByte1, 0x10, contribution);
                        tile.AddNybble(sourceRowIndex / 2, 4 + pxOffset, twoAndThreeByte2, 0x10, contribution);
                    }

                    allTiles.Add(tile);
                    sourceRowOffset += 0x40;
                }
            }

            var levelPaletteList = new List<List<ushort>>();
            foreach(var tilePaletteList in levelTilePalettes)
            {
                var levelPalette = new List<ushort>();

                int rowLimit = (level == 3) ? 128 : 64;
                int colLimit = (level == 3) ? 64 : 128;

                for (int row = 0; row < rowLimit; row++)
                {
                    for (int col = 0; col < colLimit; col++)
                    {
                        var i = row * colLimit + col;
                        var tilePalette = tilePaletteList[i];

                        var tile = allTiles[tilePalette.TileIndex];
                        var uniqueColorIndexes = tile.GetUniqueValues();

                        var palette = mapPaletteValues[tilePalette.PaletteIndex];

                        foreach(var uniqueIndex in uniqueColorIndexes)
                        {
                            if (!tilePalette.PixelColors.Contains(palette[uniqueIndex]))
                                tilePalette.PixelColors.Add(palette[uniqueIndex]);

                            if (!levelPalette.Contains(palette[uniqueIndex]))
                            {
                                levelPalette.Add(palette[uniqueIndex]);
                            }
                        }
                    }
                }

                levelPaletteList.Add(levelPalette);
            }

            //// Now, format and print
            for (int currentLevel=1;currentLevel < 9; currentLevel++)
            {
                var line = new List<string>();
                var levelPalette = levelPaletteList[currentLevel - 1];
                var tilePaletteList = levelTilePalettes[currentLevel - 1];
                var uniquePalettes = tilePaletteList.Select(p => p.PaletteIndex).Distinct().OrderBy(p => p).ToList();
                line.Add($"Level {currentLevel} (map); unique palletes used: {uniquePalettes.Count} unique colors used: {levelPalette.Count}");
                var sortedList = levelPalette.OrderBy(v => v).ToList();

                line.Add("LEVEL PALETTES: " + string.Join(",", uniquePalettes.Select(v => $"{v:X1}")));
                line.Add("LEVEL COLORS: " + string.Join(",", sortedList.Select(v => $"{v:X3}")));
                line.Add(Environment.NewLine + "TILES:");

                int rowLimit = (level == 3) ? 128 : 64;
                int colLimit = (level == 3) ? 64 : 128;

                var levelColors = new List<ushort>();
                var tilesUsed = new List<string>();

                for (int row = 0; row < rowLimit; row++)
                {
                    var rowstring = $"Row {row+1}: " + Environment.NewLine;
                    for (int col = 0; col < colLimit; col++)
                    {
                        var i = row * colLimit + col;
                        var tilePalette = tilePaletteList[i];
                        var flip = tilePalette.Flipped ? " (FL);" : "     ;";
                        var pal = $";PAL: {tilePalette.PaletteIndex:X1};";

                        var colors = string.Join(",", tilePalette.PixelColors.OrderBy(v => v).Select(v => $"{v:X3}"));

                        levelColors.AddRange(tilePalette.PixelColors);
                        var tileName = $"0x{tilePalette.TileIndex:X4}:{tilePalette.PaletteIndex:X1}";

                        tilesUsed.Add(tileName);

                        rowstring += $"  {col:D3}    TILE 0x{tilePalette.TileIndex:X4} {pal}{flip} Colors: {tilePalette.PixelColors.Count} - {colors}" + Environment.NewLine;
                    }

                    line.Add(rowstring);
                }

                var uniqueTilesForLevel = tilesUsed.Distinct().OrderBy(v => v).ToList();
                var tileList = new List<string>();

                foreach(var tile in uniqueTilesForLevel)
                {
                    tileList.Add($"TILE {tile} USED {tilesUsed.Where(c => c == tile).Count().ToString()} times");
                }
                line.Add(Environment.NewLine);
                line.Add(string.Join(Environment.NewLine, tileList));

                var uniqueColorsForLevel = levelColors.Distinct().OrderBy(v => v).ToList();
                var colorList = new List<string>();
                foreach(var color in uniqueColorsForLevel)
                {
                    colorList.Add($"COLOR {color:X3}: used {levelColors.Where(c => c == color).Count().ToString()} times");
                }

                line.Add(Environment.NewLine);
                line.Add(string.Join(Environment.NewLine, colorList));

                output.Add(string.Join(Environment.NewLine, line));
            }

            foreach (var line in output)
            {
                WriteLine(line);
            }

            WriteLine("Press any key to end");
            Console.ReadKey();
        }

        static void InterleaveBitplanes()
        {
            var dir = Directory.GetCurrentDirectory() ?? string.Empty;
            var list = new List<string>();

            FileIO.ForEachFileIn(dir, file =>
            {
                var filename = Path.GetFileName(file);

                if (filename.ToLower().StartsWith("bp") && filename.ToLower().EndsWith(".bin"))
                {
                    if (!list.Contains(file))
                        list.Add(file);
                }
            });

            list = list.OrderByDescending(e => e).ToList();

            var bytes3 = new List<byte>();
            var bytes2 = new List<byte>();
            var bytes1 = new List<byte>();
            var bytes0 = new List<byte>();
            var outputBytes = new List<byte>();

            if(list.Count == 4)
            {
                bytes3 = File.ReadAllBytes(list[0]).ToList();
                bytes2 = File.ReadAllBytes(list[1]).ToList();
                bytes1 = File.ReadAllBytes(list[2]).ToList();
                bytes0 = File.ReadAllBytes(list[3]).ToList();
            }

            for(int i = 0; i < 1280; i+=40)
            {
                outputBytes.AddRange(bytes3.Skip(i).Take(40));
                outputBytes.AddRange(bytes2.Skip(i).Take(40));
                outputBytes.AddRange(bytes1.Skip(i).Take(40));
                outputBytes.AddRange(bytes0.Skip(i).Take(40));
            }

            //var encodeKey = new List<byte>();

            //WriteLine("Files to encode: " + list.Count.ToString());

            //list.ForEach(filename => bytes.AddRange(ReadBytes(filename)));
            //encodeKey.AddRange(ReadBytes(encoderFilename));

            //WriteLine("Bytes to encode: " + bytes.Count().ToString());

            //var results = new List<byte>();

            //results = bytes.Select(b => EncodeByte(encodeKey, b)).ToList();

            File.WriteAllBytes("interleavedpic.bin", outputBytes.ToArray());

            WriteLine("Press any key to end");
            Console.ReadKey();

        }


        static void ConvertBlackTigerBitmap()
        {
            var dir = Directory.GetCurrentDirectory() ?? string.Empty;
            var list = new List<string>();
            var output = new List<string>();

            FileIO.ForEachFileIn(dir, file =>
            {
                var filename = Path.GetFileName(file);

                if (filename.ToLower().StartsWith("gfx2.bin"))
                {
                    if (!list.Contains(file))
                        list.Add(file);
                }
            });

            var sheet = new List<SixteenBySixteenTile>();
            var sheetBp1 = new List<SixteenBySixteenTile>();
            var sheetBp2 = new List<SixteenBySixteenTile>();
            var sheetBp3 = new List<SixteenBySixteenTile>();
            var sheetBp4 = new List<SixteenBySixteenTile>();
            var bytes = new List<byte>();
            var sheetRows = 0x80;
            var sheetColumns = 16;

            var lowerBitplanesOffset = 0x20000;

            foreach (var file in list)
            {
                bytes = File.ReadAllBytes(file).ToList();
            }

            int sourceRowOffset = 0;
            for(int sheetRow = 0; sheetRow < sheetRows; sheetRow++)
            {
                for (int sheetCol = 0; sheetCol < sheetColumns; sheetCol++)
                {
                    var tile = new SixteenBySixteenTile();
                    var tileBp1 = new SixteenBySixteenTile();
                    var tileBp2 = new SixteenBySixteenTile();
                    var tileBp3 = new SixteenBySixteenTile();
                    var tileBp4 = new SixteenBySixteenTile();

                    // Now, we need to iterate over the rows and columns of BITS
                    for (int sourceRowIndex = 0; sourceRowIndex < 0x40; sourceRowIndex += 2)
                    {
                        var pxOffset = sourceRowIndex < 0x20 ? 0 : 0x8;
                        var zeroAndOneByte1 = bytes[lowerBitplanesOffset + sourceRowOffset + sourceRowIndex];
                        var zeroAndOneByte2 = bytes[lowerBitplanesOffset + sourceRowOffset + sourceRowIndex + 1];
                        var twoAndThreeByte1 = bytes[sourceRowIndex + sourceRowOffset];
                        var twoAndThreeByte2 = bytes[sourceRowIndex + sourceRowOffset + 1];

                        //WriteLine($"{(lowerBitplanesOffset + sourceRowOffset + sourceRowIndex):X2}, {(lowerBitplanesOffset + sourceRowOffset + sourceRowIndex + 1):X2}, {(sourceRowIndex + sourceRowOffset):X2}, {(sourceRowIndex + sourceRowOffset + 1):X2}");

                        //Bitplane 01 - lower nybble
                        uint contribution = 8;
                        tile.AddNybble(sourceRowIndex / 2, 0 + pxOffset, zeroAndOneByte1, 0x1, contribution);
                        tile.AddNybble(sourceRowIndex / 2, 4 + pxOffset, zeroAndOneByte2, 0x1, contribution);
                        tileBp1.AddNybble(sourceRowIndex / 2, 0 + pxOffset, zeroAndOneByte1, 0x1, contribution);
                        tileBp1.AddNybble(sourceRowIndex / 2, 4 + pxOffset, zeroAndOneByte2, 0x1, contribution);
                        //tileBp1.SetNybble(sourceRowIndex / 2, 0 + pxOffset, zeroAndOneByte1, 0x1);
                        //tileBp1.SetNybble(sourceRowIndex / 2, 4 + pxOffset, zeroAndOneByte2, 0x1);
                        tileBp1.SetByte(sourceRowIndex / 2, zeroAndOneByte1, zeroAndOneByte2, 0x1);

                        //Bitplane 00 - upper nybble
                        contribution /= 2;
                        tile.AddNybble(sourceRowIndex / 2, 0 + pxOffset, zeroAndOneByte1, 0x10, contribution);
                        tile.AddNybble(sourceRowIndex / 2, 4 + pxOffset, zeroAndOneByte2, 0x10, contribution);
                        tileBp2.AddNybble(sourceRowIndex / 2, 0 + pxOffset, zeroAndOneByte1, 0x10, contribution);
                        tileBp2.AddNybble(sourceRowIndex / 2, 4 + pxOffset, zeroAndOneByte2, 0x10, contribution);
                        //tileBp2.SetNybble(sourceRowIndex / 2, 0 + pxOffset, zeroAndOneByte1, 0x10);
                        //tileBp2.SetNybble(sourceRowIndex / 2, 4 + pxOffset, zeroAndOneByte2, 0x10);
                        tileBp2.SetByte(sourceRowIndex / 2, zeroAndOneByte1, zeroAndOneByte2, 0x10);

                        //Bitplane 03 - lower nybble
                        contribution /= 2;
                        tile.AddNybble(sourceRowIndex / 2, 0 + pxOffset, twoAndThreeByte1, 0x1, contribution);
                        tile.AddNybble(sourceRowIndex / 2, 4 + pxOffset, twoAndThreeByte2, 0x1, contribution);
                        tileBp3.AddNybble(sourceRowIndex / 2, 0 + pxOffset, twoAndThreeByte1, 0x1, contribution);
                        tileBp3.AddNybble(sourceRowIndex / 2, 4 + pxOffset, twoAndThreeByte2, 0x1, contribution);
                        //tileBp3.SetNybble(sourceRowIndex / 2, 0 + pxOffset, twoAndThreeByte1, 0x1);
                        //tileBp3.SetNybble(sourceRowIndex / 2, 4 + pxOffset, twoAndThreeByte2, 0x1);
                        tileBp3.SetByte(sourceRowIndex / 2, twoAndThreeByte1, twoAndThreeByte2, 0x1);

                        //Bitplane 02 - upper nybble
                        contribution /= 2;
                        tile.AddNybble(sourceRowIndex / 2, 0 + pxOffset, twoAndThreeByte1, 0x10, contribution);
                        tile.AddNybble(sourceRowIndex / 2, 4 + pxOffset, twoAndThreeByte2, 0x10, contribution);
                        tileBp4.AddNybble(sourceRowIndex / 2, 0 + pxOffset, twoAndThreeByte1, 0x10, contribution);
                        tileBp4.AddNybble(sourceRowIndex / 2, 4 + pxOffset, twoAndThreeByte2, 0x10, contribution);
                        //tileBp4.SetNybble(sourceRowIndex / 2, 0 + pxOffset, twoAndThreeByte1, 0x10);
                        //tileBp4.SetNybble(sourceRowIndex / 2, 4 + pxOffset, twoAndThreeByte2, 0x10);
                        tileBp4.SetByte(sourceRowIndex / 2, twoAndThreeByte1, twoAndThreeByte2, 0x10);
                    }

                    sheet.Add(tile);
                    sheetBp1.Add(tileBp1);
                    sheetBp2.Add(tileBp2);
                    sheetBp3.Add(tileBp3);
                    sheetBp4.Add(tileBp4);
                    sourceRowOffset += 0x40;
                }         
            }

            // Now, format and print
            for (int sheetRow = 0; sheetRow < sheetRows; sheetRow++)
            {
                var line = new List<string>();
                for (int sheetCol = 0; sheetCol < sheetColumns; sheetCol++)
                {
                    line.Add($"TILE 0x{(sheetRow * 0x10 + sheetCol):X3}" + Environment.NewLine + sheet[sheetRow * 0x10 + sheetCol].ToString());
                }

                output.Add(string.Join(Environment.NewLine + Environment.NewLine, line));
                output.Add("-----------------------------------------------------------------");
            }

            output.Add(" ");
            output.Add("------------------------------------------ [BITPLANE 1] ------------------------------------------");

            for (int sheetRow = 0; sheetRow < sheetRows; sheetRow++)
            {
                var line = new List<string>();
                for (int sheetCol = 0; sheetCol < sheetColumns; sheetCol++)
                {
                    line.Add($"TILE 0x{(sheetRow * 0x10 + sheetCol):X3}" + Environment.NewLine + sheetBp1[sheetRow * 0x10 + sheetCol].convertBytes("8"));
                    line.Add(Environment.NewLine + sheetBp1[sheetRow * 0x10 + sheetCol].Encoded() + Environment.NewLine);
                }

                output.Add(string.Join(Environment.NewLine + Environment.NewLine, line));
                output.Add("-----------------------------------------------------------------");
            }

            output.Add(" ");
            output.Add("------------------------------------------ [BITPLANE 0] ------------------------------------------");

            for (int sheetRow = 0; sheetRow < sheetRows; sheetRow++)
            {
                var line = new List<string>();
                for (int sheetCol = 0; sheetCol < sheetColumns; sheetCol++)
                {
                    line.Add($"TILE 0x{(sheetRow * 0x10 + sheetCol):X3}" + Environment.NewLine + sheetBp2[sheetRow * 0x10 + sheetCol].convertBytes("4"));
                    line.Add(Environment.NewLine + sheetBp2[sheetRow * 0x10 + sheetCol].Encoded() + Environment.NewLine);
                }

                output.Add(string.Join(Environment.NewLine + Environment.NewLine, line));
                output.Add("-----------------------------------------------------------------");
            }

            output.Add(" ");
            output.Add("------------------------------------------ [BITPLANE 3] ------------------------------------------");

            for (int sheetRow = 0; sheetRow < sheetRows; sheetRow++)
            {
                var line = new List<string>();
                for (int sheetCol = 0; sheetCol < sheetColumns; sheetCol++)
                {
                    line.Add($"TILE 0x{(sheetRow * 0x10 + sheetCol):X3}" + Environment.NewLine + sheetBp3[sheetRow * 0x10 + sheetCol].convertBytes("2"));
                    line.Add(Environment.NewLine + sheetBp3[sheetRow * 0x10 + sheetCol].Encoded() + Environment.NewLine);
                }

                output.Add(string.Join(Environment.NewLine + Environment.NewLine, line));
                output.Add("-----------------------------------------------------------------");
            }

            output.Add(" ");
            output.Add("------------------------------------------ [BITPLANE 2] ------------------------------------------");

            for (int sheetRow = 0; sheetRow < sheetRows; sheetRow++)
            {
                var line = new List<string>();
                for (int sheetCol = 0; sheetCol < sheetColumns; sheetCol++)
                {
                    line.Add($"TILE 0x{(sheetRow * 0x10 + sheetCol):X3}" + Environment.NewLine + sheetBp4[sheetRow * 0x10 + sheetCol].convertBytes("1"));
                    line.Add(Environment.NewLine + sheetBp4[sheetRow * 0x10 + sheetCol].Encoded() + Environment.NewLine);
                }

                output.Add(string.Join(Environment.NewLine + Environment.NewLine, line));
                output.Add("-----------------------------------------------------------------");
            }


            foreach (var line in output)
            {
                WriteLine(line);
            }

            //var encodeKey = new List<byte>();

            //WriteLine("Files to encode: " + list.Count.ToString());

            //list.ForEach(filename => bytes.AddRange(ReadBytes(filename)));
            //encodeKey.AddRange(ReadBytes(encoderFilename));

            //WriteLine("Bytes to encode: " + bytes.Count().ToString());

            //var results = new List<byte>();

            //results = bytes.Select(b => EncodeByte(encodeKey, b)).ToList();

            //File.WriteAllBytes("encoded_20500.bin", results.ToArray());

            WriteLine("Press any key to end");
            Console.ReadKey();

        }

        static void DecodeTileData()
        {
            var dir = Directory.GetCurrentDirectory() ?? string.Empty;
            var list = new List<string>();

            FileIO.ForEachFileIn(dir, file =>
            {
                var filename = Path.GetFileName(file);

                if (filename.ToLower().StartsWith("tile_data_all_levels.bin"))
                {
                    if (!list.Contains(file))
                        list.Add(file);
                }
            });

            var bankSize = 0x4000;
            var bank = 9;
            var level = 1;
            var offset = 0;
            var blockOffset = 0;
            var rowBlockOffset = 0;
            var rowMultiplier = 0;

            foreach (var file in list)
            {
                var bytes = File.ReadAllBytes(file).ToList();
                for (int i = level; i < 9; i++)
                {
                    //for (int whichPalette = 0; whichPalette < 16; whichPalette++)
                    //{
                    int rowLimit = (level == 3) ? 128 : 64;
                    int colLimit = (level == 3) ? 64 : 128;

                    //WriteLine("Level " + level++.ToString() + "; Bank " + bank++.ToString() + $"; Palette {whichPalette:X1}");
                    WriteLine("Level " + level++.ToString() + "; Bank " + bank++.ToString());
                    var levelBytes = bytes.Skip(offset).Take(bankSize).ToList();
                    offset += bankSize;

                    rowBlockOffset = 0;
                    for (int row = 0; row < rowLimit; row++)
                    {
                        rowMultiplier = row % 16;
                        if ((0 == rowMultiplier) && row > 0)
                        {
                            rowBlockOffset += ((level - 1) == 3) ? 0x800 : 0x1000;
                        }

                        var rowlist = new List<string>();
                        for (int col = 0; col < colLimit; col++)
                        {
                            if ((0 == col % 16) && col > 0)
                            {
                                blockOffset += 32 * 15;
                            }

                            var byteA = levelBytes[(rowMultiplier * 32) + col * 2 + blockOffset + rowBlockOffset];             //row * (colLimit * 2)
                            var byteB = levelBytes[(rowMultiplier * 32) + col * 2 + 1 + blockOffset + rowBlockOffset];         //row * (colLimit * 2)
                            var tileIndex = byteA + ((byteB & 0x7) << 8);
                            var paletteIndex = (byteB & 0x78) >> 3;
                            //var colorrgb444 = $"0x{mapPaletteValues[whichPalette][paletteIndex]:X4}";
                            var flip = (byteB & 0x80) == 0x80 ? " (FL)" : "     ";

                            //rowlist.Add($"0x{tileIndex:X3}:{paletteIndex:X1}({colorrgb444}){flip}");
                            rowlist.Add($"0x{tileIndex:X3}:{paletteIndex:X1}{flip}");
                        }

                        blockOffset = 0;
                        WriteLine(string.Join(" | ", rowlist));
                    }

                    WriteLine("-----------------------------------------------");
                    //}
                }
            }



            //var encodeKey = new List<byte>();

            //WriteLine("Files to encode: " + list.Count.ToString());

            //list.ForEach(filename => bytes.AddRange(ReadBytes(filename)));
            //encodeKey.AddRange(ReadBytes(encoderFilename));

            //WriteLine("Bytes to encode: " + bytes.Count().ToString());

            //var results = new List<byte>();

            //results = bytes.Select(b => EncodeByte(encodeKey, b)).ToList();

            //File.WriteAllBytes("encoded_20500.bin", results.ToArray());

            WriteLine("Press any key to end");
            Console.ReadKey();

        }

        static void Set(Dictionary<byte, uint> trackToAddress)
        {
            trackToAddress.Add((byte)01, 0);
            trackToAddress.Add((byte)02, 5376);
            trackToAddress.Add((byte)03, 10752);
            trackToAddress.Add((byte)04, 16128);
            trackToAddress.Add((byte)05, 21504);
            trackToAddress.Add((byte)06, 0x6900);
            trackToAddress.Add((byte)07, 0x7e00);
            trackToAddress.Add((byte)08, 0x9300);
            trackToAddress.Add((byte)09, 0xa800);
            trackToAddress.Add((byte)10, 0xbd00);
            trackToAddress.Add((byte)11, 0xd200);
            trackToAddress.Add((byte)12, 0xe700);
            trackToAddress.Add((byte)13, 0xfc00);
            trackToAddress.Add((byte)14, 0x11100);
            trackToAddress.Add((byte)15, 0x12600);
            trackToAddress.Add((byte)16, 0x13b00);
            trackToAddress.Add((byte)17, 0x15000);
            trackToAddress.Add((byte)18, 0x16500);
            trackToAddress.Add((byte)19, 0x17800);
            trackToAddress.Add((byte)20, 0x18b00);
            trackToAddress.Add((byte)21, 0x19e00);
            trackToAddress.Add((byte)22, 0x1b100);
            trackToAddress.Add((byte)23, 0x1c400);
            trackToAddress.Add((byte)24, 0x1d700);
            trackToAddress.Add((byte)25, 0x1ea00);
            trackToAddress.Add((byte)26, 0x1fc00);
            trackToAddress.Add((byte)27, 0x20e00);
            trackToAddress.Add((byte)28, 0x22000);
            trackToAddress.Add((byte)29, 0x23200);
            trackToAddress.Add((byte)30, 0x24400);
            trackToAddress.Add((byte)31, 0x25600);
            trackToAddress.Add((byte)32, 0x26700);
            trackToAddress.Add((byte)33, 0x27800);
            trackToAddress.Add((byte)34, 0x28900);
            trackToAddress.Add((byte)35, 0x29a00);
        }

        static void ExtractPrograms()
        {
            var trackToAddress = new Dictionary<byte, uint>();
            var listOfFilesToExtract = new List<CBMFile>();

            Set(trackToAddress);

            var bytes = new List<byte>();
            var dir = Directory.GetCurrentDirectory() ?? string.Empty;

            var ofd = new OpenFileDialog();

            ofd.InitialDirectory = @"I:\C64\floppies";
            ofd.Multiselect = false;

            var check = true;

            while (check)
            {
                listOfFilesToExtract.Clear();
                bytes.Clear();

                if (ofd.ShowDialog() ?? false)
                {
                    var fn = Path.GetFileNameWithoutExtension(ofd.FileName);

                    if (null != fn && fn.Length > 0)
                    {
                        var newdir = dir + @"\" + fn;
                        Directory.SetCurrentDirectory(dir);

                        bytes = ReadBytes(ofd.FileName);

                        if (null != bytes && 174848 == bytes.Count)
                        {
                            var trackSector = bytes.Skip(0x16500).Take(2).ToList();

                            while (trackSector[0] != (byte)0)
                            {
                                var trackOffset = trackToAddress[trackSector[0]];
                                var byteOffset = (int)trackOffset + trackSector[1] * 256;

                                trackSector = bytes.Skip(byteOffset).Take(2).ToList();

                                for (int i = 0; i < 256; i += 32)
                                {
                                    var file = GetFile(bytes, i + byteOffset);
                                    if (null != file.Name && string.Empty != file.Name)
                                    {
                                        WriteLine($"{file.Name}{file.Extension}: {file.blocksUsed} blocks");
                                        if (file.Extension != ".del")
                                        {
                                            listOfFilesToExtract.Add(file);
                                        }
                                    }
                                }
                            }

                            if (listOfFilesToExtract.Count > 0)
                            {
                                if (!Directory.Exists(newdir))
                                {
                                    Directory.CreateDirectory(newdir);
                                }

                                Directory.SetCurrentDirectory(newdir);

                                foreach (var file in listOfFilesToExtract)
                                {
                                    var fileBytes = GetFileBytes(trackToAddress, bytes, file.startingTrack, file.startingSector);
                                    if (fileBytes.Count > 0 && (file.Extension != string.Empty && null != file.Extension))
                                    {
                                        var name = null != file.Extension && string.Empty != file.Extension ? file.Name + file.Extension : file.Name;
                                        name = name.Replace("/", "-").Replace("*", "-").Replace(":", "-").Replace(@"\", "-").Replace(">", "-");

                                        name = System.Text.RegularExpressions.Regex.Replace(name, @"[^\u0000-\u007F]", string.Empty);

                                        name = Encoding.ASCII.GetString(
                                        Encoding.Convert(
                                            Encoding.UTF8,
                                            Encoding.GetEncoding(
                                                Encoding.ASCII.EncodingName,
                                                new EncoderReplacementFallback(string.Empty),
                                                new DecoderExceptionFallback()
                                                ),
                                            Encoding.UTF8.GetBytes(name)));

                                        File.WriteAllBytes(name, fileBytes.ToArray());
                                    }
                                }

                            }
                        }
                    }
                }
                else check = false;
            }
        }

        static List<byte> GetFileBytes(Dictionary<byte, uint> trackToAddress, List<byte> bytes, byte track, byte sector)
        {
            var returnBytes = new List<byte>();
            if(track > 0x0 && track < 0x24)
            {
                var offset = (int)trackToAddress[track] + sector * 256;
                var trackSector = bytes.Skip(offset).Take(2).ToList();
                
                do
                {
                    track = trackSector[0];
                    sector = trackSector[1];

                    var bytesToAdd = track != 0x0 ? 254 : Math.Max(0, (int)sector -1);
                    if(bytesToAdd > 0)
                    {
                        returnBytes.AddRange(bytes.Skip(offset + 2).Take(bytesToAdd));
                    }

                    if (0x0 != track)
                    {
                        if (trackToAddress.ContainsKey(track))
                        {
                            offset = (int)trackToAddress[track] + sector * 256;
                            trackSector = bytes.Skip(offset).Take(2).ToList();
                        }
                        else track = 0x0;
                    }

                } while (track != 0x0);
            }

            return returnBytes;
        }

        static CBMFile GetFile(List<byte> bytes, int byteOffset)
        {
            var returnValue = new CBMFile();
            returnValue.filenameBytes = new List<byte>();

            byte type = bytes.Skip(byteOffset + 2).Take(1).First();

            if (type == 0x82 || type == 0xc2)
                returnValue.Extension = ".prg";

            if (type == 0x81 || type == 0xc1)
                returnValue.Extension = ".seq";

            if (type == 0x80 || type == 0xc0)
                return returnValue;

            if (type == 0x0)
                returnValue.Extension = ".del";

            returnValue.startingTrack = bytes.Skip(byteOffset + 3).Take(1).First();
            returnValue.startingSector = bytes.Skip(byteOffset + 4).Take(1).First();

            var nameBytes = bytes.Skip(byteOffset + 5).Take(25).ToList();
            returnValue.blocksUsed = bytes.Skip(byteOffset + 30).Take(1).First();

            var special = false;
            for (int i = 0; i < 25; i++)
            {
                if (nameBytes[i] == 0xa0)
                    nameBytes[i] = 0x0;

                if(nameBytes[i] < 0x20 && nameBytes[i] != 0x0)
                {
                    nameBytes[i] = (byte)(nameBytes[i] | 0x40);
                    special = true;
                }
            }

            if (special)
                nameBytes.Insert(0, 0x28);

            if (nameBytes.Contains(0x0))
            {
                var instance = nameBytes.IndexOf(0x0);
                nameBytes = nameBytes.Take(instance).ToList();

                returnValue.filenameBytes.AddRange(nameBytes);
            }

            returnValue.Name = Encoding.Default.GetString(nameBytes.ToArray());

            return returnValue;
        }

        static void EncodeDisk()
        {
            var dir = Directory.GetCurrentDirectory() ?? string.Empty;

            var list = new List<string>();
            var encoderFilename = string.Empty;

            FileIO.ForEachFileIn(dir, file =>
            {
                var filename = Path.GetFileName(file);

                if (filename.ToLower().StartsWith("to_encode_20500.bin") && !filename.ToLower().EndsWith(".bak"))
                {
                    if (!list.Contains(file))
                        list.Add(file);
                }

                if (filename.ToLower().StartsWith("encoder.bin"))
                {
                    encoderFilename = Path.GetFileName(file);
                }
            });

            var encodeKey = new List<byte>();
            var bytes = new List<byte>();

            WriteLine("Files to encode: " + list.Count.ToString());

            list.ForEach(filename => bytes.AddRange(ReadBytes(filename)));
            encodeKey.AddRange(ReadBytes(encoderFilename));

            WriteLine("Bytes to encode: " + bytes.Count().ToString());

            var results = new List<byte>();

            results = bytes.Select(b => EncodeByte(encodeKey, b)).ToList();

            File.WriteAllBytes("encoded_20500.bin", results.ToArray());

            WriteLine("Press any key to end");

            Console.ReadKey();

        }

        static byte EncodeByte(List<byte> key , byte v)
        {
            return key.Contains(v) ? Convert.ToByte(key.IndexOf(v)) : (byte)0;
        }

        static void PackDisk()
        {
            var dir = Directory.GetCurrentDirectory() ?? string.Empty;

            var list = new List<string>();

            FileIO.ForEachFileIn(dir, file =>
            {
                var filename = Path.GetFileName(file);

                if (filename.ToLower().StartsWith("progdata.bin"))
                {
                    list.Add(file);
                }

            });

            var bytes = new List<byte>();
            list.ForEach(filename => bytes.AddRange(ReadBytes(filename)));

            var results = new List<byte>();

            int offset = 0;
            for (byte track = 20; track < 27; track++)
            {
                for (byte sector = 0; sector < 19; sector++)
                {
                    if (track == 20 && sector == 0)
                        continue;

                    if (track == 25 && sector == 18)
                        continue;

                    if (track == 26 && sector == 9)
                        break;

                    results.Add(track);
                    results.Add(sector);

                    results.AddRange(bytes.Skip(offset).Take(254));
                    offset += 254;
                }
            }

            File.WriteAllBytes("packed.bin", results.ToArray());
        }

        static void Encode()
        {
            var dir = Directory.GetCurrentDirectory() ?? string.Empty;

            var list = new List<string>();

            FileIO.ForEachFileIn(dir, file =>
            {
                var filename = Path.GetFileName(file);

                if (filename.ToLower().StartsWith("cc_screen.bin"))
                {
                    list.Add(file);
                }

                if (filename.ToLower().StartsWith("cc_chars.bin"))
                {
                    list.Add(file);
                }
            });

            var bytes = new List<byte>();
            list.ForEach(filename => bytes.AddRange(ReadBytes(filename)));

            var bmp = bytes.Skip(1000).Take(8000).ToList();
            var chars = bytes.Take(1000).ToList();

            var encodedBmp = Encode(bmp);
            var encodedChars = Encode(chars);

            File.WriteAllBytes("encoded_cc.bin", encodedBmp.Concat(encodedChars).ToArray());
        }

        static List<byte> ReadBytes(string filename)
        {
            return File.ReadAllBytes(filename).ToList();
        }

        static List<byte> Encode(List<byte> source)
        {
            var list = new List<byte>();

            byte current = 0;
            var count = 0;

            var encoded = 0;
            source.ForEach(b =>
            {
                encoded++;

                if (list.Count == 0 && count == 0)
                {
                    current = b;
                }

                if (b == current)
                {
                    count++;
                    if(256 == count)
                    {
                        count = 1;
                        list.Add((byte)255);
                        list.Add(current);
                    }
                }

                else
                {
                    list.Add((byte)count);
                    list.Add(current);
                    count = 1;
                    current = b;
                }

                if (encoded == source.Count)
                {
                    list.Add((byte)count);
                    list.Add(current);
                }
            });

            list.Add((byte)0);

            return list;
        }

        static void DoRoomDescs(string[] args)
        {
            var finalResults = new Dictionary<string, string>();
            
            //var logger = new LightLogger();

            var jsonFolder = Directory.GetCurrentDirectory() ?? string.Empty;

            //if (!Directory.Exists("testlogs"))
            //    Directory.CreateDirectory("testlogs");

            WriteLine("Press any key to start");

            Console.ReadKey();

            //logger.Log($"======================================== Process start: {DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss.fff", System.Globalization.CultureInfo.InvariantCulture)}");

            var dir = jsonFolder;
            //var list = new List<string>();
            //var list2 = new List<string>();
            //var list3 = new List<string>();

            //var list4 = new List<string>();
            
            //var list5 = new List<string>();

            var list6 = new List<string>();

            var list7 = new List<string>();

            var diskfile = string.Empty;

            FileIO.ForEachFileIn(jsonFolder, file => 
            {
                //    if (file.ToLower().EndsWith(".d64"))
                //        list.Add(file);

                //    if (file.ToLower().EndsWith("binary.txt"))
                //        list2.Add(file);

                //    if (file.ToLower().EndsWith("exod.txt"))
                //        list3.Add(file);

                //    if (file.ToLower().EndsWith("exod.dat"))
                //        list4.Add(file);

                var filename = Path.GetFileName(file);

                //if (filename.ToLower().StartsWith("program_data_"))
                //{
                //    list5.Add(file);
                //}

                if (filename.ToLower().StartsWith("rooms_and_codes"))
                {
                    list7.Add(file);
                }

                if (filename.ToLower().Equals("rooms.bin"))
                {
                    list6.Add(file);
                }

                if (filename.Equals("001_exod.d64"))
                    diskfile = file;
            });


            //var config = new Configuration(logger, "cmsettings.json");

            //list.Sort();
            //list2.Sort();

            //list5.Sort();
            list6.Sort();

            var diskDescriptions = new Dictionary<string, List<byte>>();
            var diskData = new Dictionary<string, List<byte>>();
            var roomDescriptions = new Dictionary<string, string>();
            var shortDescs = new Dictionary<string, string>();
            var sortByXYZ = new List<Room>();
            var names = new List<string>();
            var roomCodes = new Dictionary<string, List<RoomCode>>();

            list6.ForEach(filename =>
            {
                try
                {
                    var bytes = File.ReadAllBytes(filename);
                    var filebytes = string.Empty == diskfile ? new byte[] { }  : File.ReadAllBytes(diskfile);

                    var p = 0;


                    list7.ForEach(filename =>
                    {
                        try
                        {
                            var codeslines = File.ReadAllLines(filename).ToList();
                            foreach (var ln in codeslines)
                            {
                                var sp = ln.Trim().Split('-').ToList();

                                if (sp.Count > 0)
                                {
                                    var codes = sp.Skip(1).ToList();
                                    var name = sp.First().Trim();

                                    roomCodes.Add(name, new List<RoomCode>());
                                    foreach (var code in codes)
                                    {
                                        var stripped = code.Replace("[", string.Empty).Replace("]", string.Empty).Trim();
                                        var numerals = stripped.Split(',').Select(c => Convert.ToInt32(c.Trim())).ToList();

                                        if (numerals.Count == 3)
                                        {
                                            roomCodes[name].Add(new RoomCode() { a = numerals[0], b = numerals[1], c = numerals[2] });
                                        }
                                    }
                                }
                            }
                        }
                        catch (Exception ex) { }//logger?.LogException(ex);
                    });

                    while (p < bytes.Length)
                    {
                        byte x = bytes[p];
                        byte y = bytes[p + 1];
                        byte z = bytes[p + 2];

                        if (x >= 32)
                            break;

                        byte track = bytes[p + 3];
                        byte sector = bytes[p + 4];

                        if (!((int)track == 32 && (int)sector == 32))
                        {

                            if (x == 5 && y == 14 && z == 0)
                            {
                                track = 7;
                                sector = 2;
                            }

                            if (x == 5 && y == 13 && z == 1)
                                sector = 7;

                            byte descr_1 = bytes[p + 5];
                            byte descr_2 = bytes[p + 6];
                            byte descr_3 = bytes[p + 7];

                            byte directions = bytes[p + 8];
                            byte updown = bytes[p + 9];

                            var list3 = GetDirection(directions);
                            var list4 = GetUpDown(updown);

                            var direction = list3.Count > 0 ? " DIR: ( " + string.Join(", ", list3) + " )" : string.Empty;
                            var up = list4.Count > 0 ? " UP/DOWN: ( " + string.Join(", ", list4) + " )" : string.Empty;

                            var baseAddress = GetBaseAddress(track, sector);
                            var baseAddressDesc = $"(on disk at ${baseAddress:X4} ({baseAddress:d5}))";

                            var description = $" ({x:d}, {y:d}, {z:d}) T/S: ({track:d},{sector:d}) {baseAddressDesc} a={descr_1:d} b={descr_2:d} c={descr_3:d}" + direction + up;
                            var name = $"({x:d}, {y:d}, {z:d})";

                            var sd = $"{name} a={descr_1:d} b={descr_2:d} c={descr_3:d}{direction}{up}";

                            var offset = GetInitialTrackOffset(filebytes, baseAddress, x, y, z);

                            var ourRoom = new Room { Name = name, x = (int)x, y = (int)y, z = (int)z, a = descr_1, b = descr_2, c = descr_3, initSectorOffset = offset, baseAddress = baseAddress };
                            ourRoom.Add(baseAddress, new TS() { track = track, sector = sector, offset = offset });

                            sortByXYZ.Add(ourRoom);

                            if (!roomDescriptions.ContainsKey(name))
                                roomDescriptions.Add(name, description);

                            else roomDescriptions[name] = description;

                            if (!shortDescs.ContainsKey(name))
                                shortDescs.Add(name, sd);

                            else shortDescs[name] = sd;

                            if (!diskDescriptions.ContainsKey(name))
                                diskDescriptions.Add(name, new List<byte>());

                            if (!diskData.ContainsKey(name))
                                diskData.Add(name, new List<byte>());

                            GetRoomDescription(diskDescriptions[name], filebytes, baseAddress, x, y, z);
                            GetRoomData(ourRoom, diskData[name], filebytes);
                        }

                        p += 10;
                    }

                    var newlist = sortByXYZ.OrderBy(r => r.z).ThenBy(r => r.x).ThenBy(r => r.y).Select(r => r).ToList();

                    //foreach (var room in newlist)// //var room in biglist //var name in roomDescriptions.Keys
                    foreach (var name in roomDescriptions.Keys)// //var room in biglist //var name in roomDescriptions.Keys
                    {
                        //var name = room.Name;
                        var a = roomDescriptions[name];

                        var r = GetRoomName(diskData[name].ToArray());

                        if(string.Empty != r)
                        {
                            var rm = sortByXYZ.Where(sb => sb.Name == name).FirstOrDefault();
                            if (null != rm)
                                rm.Desc = r;
                        }

                        var roomname = string.Empty == r ? "[UNNAMED]" + a : $"\"{GetRoomName(diskData[name].ToArray())}\" " + a;
                        WriteLine(roomname);
                    }

                    var biglist = sortByXYZ.OrderBy(r => r.Desc ?? string.Empty).ThenBy(r => r.x).ThenBy(r => r.y).ThenBy(r => r.z).Select(r => r).ToList();

                    WriteLine("\nSorted alphabetically\n");

                    foreach (var room in biglist)
                    {
                        var a = string.Empty;// roomDescriptions[room.Name];
                        var rd = string.Empty == (room.Desc ?? string.Empty) ? "[UNNAMED]" + a : $"\"{room.Desc}\" " + a;
                        WriteLine($"{rd} {room.Name}");
                    }

                    WriteLine("\nSorted by XYZ\n");

                    foreach (var room in newlist)
                    {
                        var rd = string.Empty == (room.Desc ?? string.Empty) ? "[UNNAMED]" : $"\"{room.Desc}\"";
                        WriteLine($"{room.Name}\t{rd}");
                    }

                    WriteLine(" ");

                    var tslist = sortByXYZ.OrderBy(r => r.startingTrack).ThenBy(r => r.startingSector).ThenBy(r => r.x).ThenBy(r => r.y).ThenBy(r => r.z).Select(r => r).ToList();

                    WriteLine("\nSorted by Track & Sector\n");

                    foreach (var room in tslist)
                    {
                        var rd = string.Empty == (room.Desc ?? string.Empty) ? "[UNNAMED]" : $"\"{room.Desc}\"";
                        WriteLine($"({room.startingTrack:d},{room.startingSector:d})\t{room.Name}\t{rd}");
                    }

                    WriteLine(" ");

                    foreach (var name in roomDescriptions.Keys)
                    {
                        var r = GetRoomName(diskData[name].ToArray());
                        var rd = string.Empty == (r ?? string.Empty) ? "[UNNAMED] " : $"\"{GetRoomName(diskData[name].ToArray())}\" ";
                        var roomname = rd + name;
                        WriteLine(roomname);
                    }

                    WriteLine(" ");

                    foreach (var name in roomDescriptions.Keys)
                    {
                        var r = GetDescription(diskDescriptions[name].ToArray());

                        if (string.Empty != r)
                        {
                            WriteLine($"\"{GetRoomName(diskData[name].ToArray())}\" " + shortDescs[name]);
                            WriteLine($"{r}\n");
                        }
                        else
                        {
                            WriteLine($"[UNNAMED] {shortDescs[name]}");
                            WriteLine($"NO ROOM DATA\n");
                        }
                    }

                    WriteLine("\nAnd now, the mon juste...\n");

                    var errorDic = new Dictionary<string, List<string>>();

                    foreach (var name in roomDescriptions.Keys)
                    {
                        var room = sortByXYZ.Where(sb => sb.Name == name).FirstOrDefault();

                        if (!errorDic.ContainsKey(name))
                            errorDic.Add(name, new List<string>());

                        if (room.x == 6 && room.y == 5 && room.z == 6)
                        {
                            //var t = 0;
                            //while(t + 15 < diskData[name].Count)
                            //{
                            //    for(int v = 0; v < 16; v++)
                            //    {
                            //        Write($"{diskData[name][v + t]:X2} ");
                            //    }

                            //    WriteLine(" ");

                            //    t += 16;
                            //}

                            //for(int v = t; v < diskData[name].Count; v++)
                            //{
                            //    Write($"{diskData[name][v]:X2} ");
                            //}

                            //WriteLine(" ");
                        }

                        var r = null != room ? GetFullDescription(errorDic[name], diskData[name].ToArray(), room.a, room.b, room.c) : string.Empty;

                        if (string.Empty != r)
                        {
                            WriteLine($"\"{GetRoomName(diskData[name].ToArray())}\" T/S: ({room.startingTrack},{room.startingSector}) " + shortDescs[name]);
                            WriteLine($"{r}\n");
                        }
                        else
                        {
                            WriteLine($"[UNNAMED] {shortDescs[name]}");
                            WriteLine($"NO ROOM DATA\n");
                        }
                    }

                    WriteLine("\nDESCRIPTOR CODES\n");

                    var codes = new Dictionary<string, List<string>>();

                    foreach (var name in roomDescriptions.Keys)
                    {
                        var room = sortByXYZ.Where(sb => sb.Name == name).FirstOrDefault();

                        if(room.x == 2 && room.y == 3 && room.z == 5)
                        {

                        }

                        if (!codes.ContainsKey(name))
                            codes.Add(name, new List<string>());

                        GetCodes(room, $"\"{room.Desc}\" {name} a={room.a}, b={room.b}, c={room.c} ({room.startingTrack:d}, {room.startingSector:d}:[${room.initSectorOffset:X2}] ({room.startingSector:d}:[{room.initSectorOffset:d3}]))", roomCodes, diskData[name].ToArray());
                    }


                    WriteLine("\nFull descriptions--all combinations...\n");
                    var errorDic2 = new Dictionary<string, List<string>>();

                    foreach (var name in roomDescriptions.Keys)
                    {
                        var room = sortByXYZ.Where(sb => sb.Name == name).FirstOrDefault();

                        if (!errorDic2.ContainsKey(name))
                            errorDic2.Add(name, new List<string>());

                        if (room.x == 5 && room.y == 8 && room.z == 2)
                        {
                            //var t = 0;
                            //while(t + 15 < diskData[name].Count)
                            //{
                            //    for(int v = 0; v < 16; v++)
                            //    {
                            //        Write($"{diskData[name][v + t]:X2} ");
                            //    }

                            //    WriteLine(" ");

                            //    t += 16;
                            //}

                            //for(int v = t; v < diskData[name].Count; v++)
                            //{
                            //    Write($"{diskData[name][v]:X2} ");
                            //}

                            //WriteLine(" ");
                        }

                        var rcodes = roomCodes.ContainsKey(name) ? roomCodes[name] : new List<RoomCode>();
                        var allCodesString = " " + string.Join(" ", rcodes.Select(c => $"[{c.a},{c.b},{c.c}]"));

                        WriteLine($"\"{GetRoomName(diskData[name].ToArray())}\" T/S: ({room.startingTrack},{room.startingSector}) " + shortDescs[name] + allCodesString);
                        WriteLine("---------------------------------------------------------------------------------------------------");

                        foreach (var rcode in rcodes)
                        {
                            var bytelist = string.Empty;
                            if (room.x == 5 && room.y == 8 && room.z == 2)
                            {
                                //bytelist = string.Join(" ", diskData[name].Select(b => $"{b:X2}"));
                            }
                            if (room.x == 5 && room.y == 14 && room.z == 0 && rcode.a == 2 && rcode.b == 0)
                            {

                            }
                            var r = null != room ? GetFullDescription(errorDic2[name], diskData[name].ToArray(), rcode.a, rcode.b, rcode.c) : string.Empty;

                            if (string.Empty != r)
                            {
                                WriteLine($"Room code combo: [{rcode.a},{rcode.b},{rcode.c}]");
                                WriteLine($"{r}\n");
                                WriteLine("----------------------------------------");
                            }
                            else
                            {
                                WriteLine($"NO ROOM DATA\n");
                            }
                        }
                    }

                    WriteLine("\nCODE WORKSHEET\n");

                    WriteLine("---------------------------------------------------------------------------------------------------");
                    foreach (var name in roomDescriptions.Keys)
                    {
                        var room = sortByXYZ.Where(sb => sb.Name == name).FirstOrDefault();

                        var rcodes = roomCodes.ContainsKey(name) ? roomCodes[name] : new List<RoomCode>();

                        var i = 0;
                        foreach(var rcode in rcodes)
                        {
                            WriteLine($"{name} [{rcode.a},{rcode.b},{rcode.c}] -- " + ((i++ == 0) ? "(default)" : string.Empty));
                        }

                        WriteLine("---------------------------------------------------------------------------------------------------");
                    }

                    WriteLine("\nAnd now, errors...\n");

                    foreach (var name in errorDic.Keys)
                    {
                        if (errorDic[name].Count > 0)
                        {
                            var room = sortByXYZ.Where(sb => sb.Name == name).FirstOrDefault();
                            WriteLine($"ERROR: \"{room.Desc}\" {name} ({room.startingTrack},{room.startingSector}) a={room.a}, b={room.b}, c={room.c}");
                            foreach (var error in errorDic[name])
                            {
                                WriteLine($"     {error}");
                            }

                            WriteLine(" ");
                        }
                    }
                }
                catch (Exception ex) { }//logger?.LogException(ex);
            });



            //var setOne = 21 * 256;
            //var setTwo = 19 * 256;
            //var setThree = 18 * 256;
            //var setFour = 17 * 256;

            //var baseAddress = 0;

            //for (int i=1;i < 18; i++)
            //{
            //    for (int j = 0; j < 21; j++)
            //    {
            //        WriteLineAddress(i, j, baseAddress);
            //        baseAddress += 256;
            //    }
            //}

            //for(int i = 18; i < 25; i++)
            //{
            //    for (int j = 0; j < 19; j++)
            //    {
            //        WriteLineAddress(i, j, baseAddress);
            //        baseAddress += 256;
            //    }
            //}

            //for (int i = 25; i < 31; i++)
            //{
            //    for (int j = 0; j < 18; j++)
            //    {
            //        WriteLineAddress(i, j, baseAddress);
            //        baseAddress += 256;
            //    }
            //}

            //for (int i = 31; i < 36; i++)
            //{
            //    for (int j = 0; j < 17; j++)
            //    {
            //        WriteLineAddress(i, j, baseAddress);
            //        baseAddress += 256;
            //    }
            //}



            //var dic = new Dictionary<int, string>();
            //dic.Add(7936, "noun_info");                 // $000
            //dic.Add(8296, "descflag_t_s_buffer");       // $168
            //dic.Add(8808, "weight_size_table");         // $368
            //dic.Add(8988, "room_info");                 // $41C
            //dic.Add(10988, "word_dictionary");          // $BEC
            //dic.Add(15988, "verb_lexicon");             // $1F74
            //dic.Add(25600, "game_var_lexicon");         // $4500
            //dic.Add(28672, "variables");                // $5100

            //list5.ForEach(filename =>
            //{
            //    try
            //    {
            //        var name = Path.GetFileNameWithoutExtension(filename);
            //        logger.Log("File: " + filename);

            //        var bytes = File.ReadAllBytes(filename);

            //        GetWordDictionary(bytes.Skip(360 + 512 + 180 + 2000).Take(5000).ToArray());

            //        WriteLine("");
            //        WriteLine(";  $1F00 (7936) Noun Info [4X4]");
            //        WriteLine("noun_info");
            //        WriteLine("");

            //        PrintNounInfo(bytes.Take(360).ToArray());

            //        WriteLine("");
            //        WriteLine(";  $2068 (8296) DESCFLAG T,S BUFFER");
            //        WriteLine("descflag_t_s_buffer");
            //        WriteLine("");

            //        PrintDescflag(bytes.Skip(360).Take(512).ToArray());

            //        WriteLine("");
            //        WriteLine(";  $2268 (8808) SIZE/WEIGHT TABLE [2X2] (W,S)");
            //        WriteLine("weight_size_table");
            //        WriteLine("");

            //        PrintNounSizeWeight(bytes.Skip(360 + 512).Take(180).ToArray());

            //        WriteLine("");
            //        WriteLine(";  $231C (8988) RM INFO [10x10] L,L,L,T,S,D,D,D,Dr,U/D");
            //        WriteLine("room_info");
            //        WriteLine("");

            //        PrintRoomInfo(bytes.Skip(360 + 512 + 180).Take(2000).ToArray());

            //        WriteLine("");
            //        WriteLine(";  $2AEC (10988) WORD DICTIONARY (5000 CHARS)");
            //        WriteLine("word_dictionary");
            //        WriteLine("");

            //        PrintWordDictionary(bytes.Skip(360 + 512 + 180 + 2000).Take(5000).ToArray());

            //        WriteLine("");
            //        WriteLine(";  $3E74 (15988) VERB LEXICON (9612 BYTES)");
            //        WriteLine("verb_lexicon");
            //        WriteLine("");

            //        PrintVerbLexicon(bytes.Skip(360 + 512 + 180 + 2000 + 5000).Take(9612).ToArray());

            //        WriteLine("");
            //        WriteLine(";  $6400 (25600) GAME VAR LEXICON (3072 BYTES)");
            //        WriteLine("game_var_lexicon");
            //        WriteLine("");

            //        PrintGameVarLexicon(bytes.Skip(360 + 512 + 180 + 2000 + 5000 + 9612).Take(3072).ToArray());

            //        WriteLine("");
            //        WriteLine(";  $7000 (28672) VARIABLES (512 BYTES)");
            //        WriteLine("variable_space");
            //        WriteLine("");

            //        PrintVariables(bytes.Skip(360 + 512 + 180 + 2000 + 5000 + 9612 + 3072).ToArray());
            //    }
            //    catch (Exception ex) { logger?.LogException(ex); }
            //});



            //list.ForEach(filename =>
            //{
            //    try
            //    {
            //        var name = Path.GetFileNameWithoutExtension(filename);
            //        logger.Log("File: " + filename);

            //        var disk = new CBMDisk(name);
            //        disk.Open(filename);

            //        logger.Log(disk.GetDecimal());

            //        logger.Log("Now hex");
            //        logger.Log(disk.GetHex());

            //        logger.Log("Now ASCII");
            //        logger.Log(disk.GetASCII());
            //    }
            //    catch (Exception ex) { logger?.LogException(ex); }

            //});

            //list2.ForEach(filename =>
            //{
            //    var lines = File.ReadAllLines(filename).ToList();
            //    var bytes = new List<byte>();

            //    foreach (string line in lines)
            //    {
            //        var read = line.Split(' ').Where(s => s.Length == 2);
            //        foreach (string b in read)
            //        {
            //            bytes.Add((byte)(Convert.ToInt32(b, 16)));
            //        }
            //    }

            //    for (int i = 0; i < 15; i++)
            //        bytes.Insert(i, (byte)0);

            //    File.WriteAllBytes("something4.dat", bytes.ToArray());
            //});

            //list3.ForEach(filename =>
            //{
            //    var lines = File.ReadAllLines(filename).ToList();
            //    var bytes = new List<byte>();

            //    var plain = Path.GetFileNameWithoutExtension(filename);
            //    var start = plain.Split('_').FirstOrDefault();

            //    foreach (string line in lines)
            //    {
            //        var l = line;
            //        l = l.Replace(".", ",");
            //        var read = l.Split(',').Where(s => s.Length>0);
            //        foreach (string b in read)
            //        {
            //            bytes.Add((byte)(Convert.ToInt32(b)));
            //        }
            //    }

            //    var val = Convert.ToInt32(start) - 2049;

            //    if (!plain.StartsWith("2049"))
            //    {
            //        for (int i = 0; i < val; i++)
            //            bytes.Insert(i, (byte)0);
            //    }

            //    File.WriteAllBytes(plain + ".dat", bytes.ToArray());
            //});

            //list4.ForEach(filename =>
            //{
            //    try
            //    {
            //        var name = Path.GetFileNameWithoutExtension(filename);
            //        logger.Log("File: " + filename);

            //        var plain = Path.GetFileNameWithoutExtension(filename);
            //        var start = plain.Split('_').FirstOrDefault();
            //        var val = 2048;// Convert.ToInt32(start);

            //        var bytearray = File.ReadAllBytes(filename);
            //        var bytes = bytearray.ToList();
            //        bytes.Insert(0, 0);
            //        var list6 = new List<string>();

            //        var counter = 0;
            //        for(int i=val; i<val+bytes.Count; i += 8, counter +=8)
            //        {
            //            var str = $"{i:d8}: " + string.Join(",", bytes.Skip(counter).Take(Math.Min(bytes.Count - counter, 8)).Select(b => ((int)(b)).ToString().Trim()));
            //            //logger.Log(str);
            //            Trace.WriteLine(str);
            //            list6.Add(str);
            //        }

            //        File.WriteAllLines(plain + "_decimal.txt", list6);
            //    }
            //    catch (Exception ex) { logger?.LogException(ex); }

            //});
        }

        private static void PrintNounInfo(byte[] bytes)
        {
            var j = 0;
            if((bytes.Length % 4) == 0)
            {
                for(int i = 0; i < bytes.Length; i += 4)
                {
                    var name = (commandList.ContainsKey((byte)((i / 4) + 25))) ? "\"" + commandList[(byte)((i / 4) + 25)] + "\"" : $"UNUSED NOUN #{(j + 25):d3}";
                    WriteLine($"; ITEM #{j:d3} {name} (${((j++) + 25):X2}): LOC: ({(bytes[i])}, {(bytes[i + 1])}, {(bytes[i + 2])}) STATUS: ({(bytes[i + 3])})");
                    WriteLine($"!byte ${(bytes[i]):X2},${(bytes[i+1]):X2},${(bytes[i+2]):X2},${(bytes[i+3]):X2}");
                    WriteLine("");
                }
            }
        }

        private static void PrintDescflag(byte[] bytes)
        {
            if ((bytes.Length % 8) == 0)
            {
                for (int i = 0; i < bytes.Length; i += 8)
                {
                    WriteLine($"!byte ${(bytes[i]):X2},${(bytes[i + 1]):X2},${(bytes[i + 2]):X2},${(bytes[i + 3]):X2},${(bytes[i + 4]):X2},${(bytes[i + 5]):X2},${(bytes[i + 6]):X2},${(bytes[i + 7]):X2}     ; ");
                }
            }
        }

        private static void PrintNounSizeWeight(byte[] bytes)
        {
            if ((bytes.Length % 2) == 0)
            {
                for (int i = 0; i < bytes.Length; i += 2)
                {
                    var name = (commandList.ContainsKey((byte)((i / 2) + 25))) ? "\"" + commandList[(byte)((i / 2) + 25)] + "\"" : $"UNUSED NOUN #{((i / 2) + 25):d3}";
                    WriteLine($"!byte ${(bytes[i]):X2},${(bytes[i + 1]):X2}     ; " + name);
                }
            }
        }

        private static void PrintRoomInfo(byte[] bytes)
        {
            if ((bytes.Length % 10) == 0)
            {
                for (int i = 0; i < bytes.Length; i += 10)
                {
                    WriteLine($"; ({(bytes[i])}, {(bytes[i+1])}, {(bytes[i+2])}) T/S ({(bytes[i + 3])}, {(bytes[i + 4])}) D,D,D ({(bytes[i + 5])}, {(bytes[i + 6])}, {(bytes[i + 7])}) Dir ({(bytes[i + 8])}) Up/Down ({(bytes[i + 9])})");
                    WriteLine($"!byte ${(bytes[i]):X2},${(bytes[i + 1]):X2},${(bytes[i + 2]):X2},${(bytes[i + 3]):X2},${(bytes[i + 4]):X2},${(bytes[i + 5]):X2},${(bytes[i + 6]):X2},${(bytes[i + 7]):X2},${(bytes[i + 8]):X2},${(bytes[i + 9]):X2}");
                    WriteLine("");
                }
            }
        }

        private static void GetWordDictionary(byte[] bytes) // three 166's mean "the end"
        {
            var i = 0;
            var keepGoing = true;

            while (i < 5000 && keepGoing)
            {
                var stopFormatting = bytes[i] == 166 && bytes[i + 1] == 166 && bytes[i + 2] == 166;

                var list = new List<string>();
                var bytelist = new List<byte>();
                var id = string.Empty;
                byte lastCommand = (byte)0;

                if (!stopFormatting)
                {
                    var printedID = false;
                    while (bytes[i] != 42)
                    {
                        if (printedID)
                        {
                            bytelist.Add(bytes[i]);
                        }

                        else
                        {
                            lastCommand = bytes[i];
                        }
                        printedID = true;
                        i++;
                    }

                    i++;
                }
                else keepGoing = false;

                var commandBytes = new List<byte>();

                if (bytelist.Count > 1)
                {
                    if (bytelist.Last() == (byte)32)
                    {
                        bytelist.RemoveAt(bytelist.Count - 1);
                    }

                    commandBytes.AddRange(bytelist);//lastCommand
                }

                if (lastCommand != (byte)(0) && commandBytes.Count > 0 && !commandList.ContainsKey(lastCommand))
                {
                    commandList.Add(lastCommand, string.Join("", commandBytes.Select(b => !Printable(b) ? "." : Encoding.ASCII.GetString(new byte[] { b }))));
                }
            }
        }

        private static void PrintWordDictionary(byte[] bytes) // three 166's mean "the end"
        {
            var i = 0;
            var keepGoing = true;

            while(i < 5000 && keepGoing)
            {
                var stopFormatting = bytes[i] == 166 && bytes[i + 1] == 166 && bytes[i + 2] == 166;

                var list = new List<string>();
                var bytelist = new List<byte>();
                var id = string.Empty;
                byte lastCommand = (byte)0;

                if (stopFormatting)
                {
                    keepGoing = false;
                    WriteLine("; end of list");
                    WriteLine($"!byte ${(bytes[i]):X2},${(bytes[i + 1]):X2},${(bytes[i + 2]):X2}");

                    i += 3;
                    while ((5000 - i) > 8)
                    {
                        WriteLine($"!byte ${(bytes[i]):X2},${(bytes[i + 1]):X2},${(bytes[i + 2]):X2},${(bytes[i + 3]):X2},${(bytes[i + 4]):X2},${(bytes[i + 5]):X2},${(bytes[i + 6]):X2},${(bytes[i + 7]):X2}");
                        i += 8;
                    }

                    while (i < 5000)
                        list.Add($"${(bytes[i++]):X2}");
                }
                else
                {
                    var printedID = false;
                    while (bytes[i] != 42)
                    {
                        if (printedID)
                        {
                            bytelist.Add(bytes[i]);
                        }

                        else
                        {
                            id = $"ID: ${(bytes[i]):X2} ({((int)bytes[i]):d3}) ";
                            lastCommand = bytes[i];
                        }
                        printedID = true;
                        list.Add($"${(bytes[i++]):X2}");
                    }

                    bytelist.Add(bytes[i]);
                    list.Add($"${(bytes[i++]):X2}");
                }

                var commandBytes = new List<byte>();

                if (bytelist.Count > 1)
                {
                    commandBytes.AddRange(bytelist);//lastCommand

                    bytelist.Insert(0, (byte)34);
                    if(bytelist.Last() == (byte)42)
                    {
                        bytelist.Insert(bytelist.Count - 1, (byte)34);
                        commandBytes.RemoveAt(commandBytes.Count - 1);

                        if (commandBytes.Last() == (byte)32)
                            commandBytes.RemoveAt(commandBytes.Count - 1);
                    }
                }

                if(lastCommand != (byte)(0) && commandBytes.Count > 0 && !commandList.ContainsKey(lastCommand))
                {
                    commandList.Add(lastCommand, string.Join("", commandBytes.Select(b => !Printable(b) ? "." : Encoding.ASCII.GetString(new byte[] { b }))));
                }

                var comment = bytelist.Count > 0 ? string.Join("", bytelist.Select(b => !Printable(b) ? "." : Encoding.ASCII.GetString(new byte[] { b }))) : string.Empty;

                if(id != string.Empty)
                {
                    if(comment != string.Empty)
                    {
                        WriteLine(";      " + id + "     " + comment);
                    }
                    else WriteLine(";      " + id);
                }
                else if(comment != string.Empty)
                {
                    WriteLine(";      " + comment);
                }

                WriteLine("!byte " + string.Join(",", list));// + "     ; " + comment
                WriteLine("");
            }
        }

        private static void PrintVerbLexicon(byte[] bytes)
        {
            var i = 0;
            var keepGoing = true;

            while (i < 9612 && keepGoing)
            {
                var stopFormatting = bytes[i] == 166;

                var list = new List<string>();
                var bytelist = new List<byte>();
                byte command = (byte)0;

                if (stopFormatting)
                {
                    keepGoing = false;
                    WriteLine("; end of list");
                    WriteLine($"!byte ${(bytes[i++]):X2}");

                    while ((9612 - i) > 8)
                    {
                        WriteLine($"!byte ${(bytes[i]):X2},${(bytes[i + 1]):X2},${(bytes[i + 2]):X2},${(bytes[i + 3]):X2},${(bytes[i + 4]):X2},${(bytes[i + 5]):X2},${(bytes[i + 6]):X2},${(bytes[i + 7]):X2}");
                        i += 8;
                    }

                    while (i < 9612)
                        list.Add($"${(bytes[i++]):X2}");
                }
                else
                {
                    command = bytes[i];
                    while (bytes[i] != 166)
                    {
                        bytelist.Add(bytes[i]);
                        list.Add($"${(bytes[i++]):X2}");
                    }

                    bytelist.Add(bytes[i]);
                    list.Add($"${(bytes[i++]):X2}");
                }

                if (commandList.ContainsKey(command))
                {
                    WriteLine($";      ID: ${(command):X2} ({((int)command):d3}) " + "\"" + commandList[command] + "\"");
                }

                var j = 0;
                while((list.Count - j) > 8)
                {
                    WriteLine("!byte " + string.Join(",", list.Skip(j).Take(8)));
                    j += 8;
                }


                WriteLine("!byte " + string.Join(",", list.Skip(j)));// + "     ; " + comment
                WriteLine("");
            }
        }

        private static void PrintGameVarLexicon(byte[] bytes)
        {
            var i = 0;
            var keepGoing = true;

            var command = 0;

            while (i < 3072 && keepGoing)
            {
                var stopFormatting = bytes[i] == 166;

                var list = new List<string>();
                var bytelist = new List<byte>();

                if (stopFormatting)
                {
                    keepGoing = false;
                    WriteLine("; end of list");
                    WriteLine($"!byte ${(bytes[i++]):X2}");

                    while ((3072 - i) > 8)
                    {
                        WriteLine($"!byte ${(bytes[i]):X2},${(bytes[i + 1]):X2},${(bytes[i + 2]):X2},${(bytes[i + 3]):X2},${(bytes[i + 4]):X2},${(bytes[i + 5]):X2},${(bytes[i + 6]):X2},${(bytes[i + 7]):X2}");
                        i += 8;
                    }

                    while (i < 3072)
                        list.Add($"${(bytes[i++]):X2}");
                }
                else
                {
                    WriteLine($"; LEXICON #{command} (${command++:X2})");
                    while (bytes[i] != 166)
                    {
                        bytelist.Add(bytes[i]);
                        list.Add($"${(bytes[i++]):X2}");
                    }

                    bytelist.Add(bytes[i]);
                    list.Add($"${(bytes[i++]):X2}");
                }

                var j = 0;
                while ((list.Count - j) > 8)
                {
                    WriteLine("!byte " + string.Join(",", list.Skip(j).Take(8)) + "     ; ");
                    j += 8;
                }


                WriteLine("!byte " + string.Join(",", list.Skip(j)) + "     ; ");
                WriteLine("");
            }
        }


        private static void PrintVariables(byte[] bytes)
        {
            var list = new List<string>();
            int i = 0;
            while ((bytes.Length - i) > 8)
            {
                WriteLine($"!byte ${(bytes[i]):X2},${(bytes[i + 1]):X2},${(bytes[i + 2]):X2},${(bytes[i + 3]):X2},${(bytes[i + 4]):X2},${(bytes[i + 5]):X2},${(bytes[i + 6]):X2},${(bytes[i + 7]):X2}     ; ");
                i += 8;
            }

            while (i < bytes.Length)
                list.Add($"${(bytes[i++]):X2}");

            WriteLine("!byte " + string.Join(",", list) + "     ; ");
            WriteLine("");
        }

        static void Write(string s)
        {
            Trace.Write(s);
            Console.Write(s);
        }

        static void WriteLine(string s)
        {
            Trace.WriteLine(s);
            Console.WriteLine(s);
        }

        static void WriteLineAddress(int i, int j, int number)
        {
            WriteLine($"{i:d2},{j:d2} (${i:X2},${j:X2}): ${number:X5} ({number})");
        }
    }

    //public List<Task> All { get => Edit ? Children.Select(n => n.Task).ToList() : GetActionTasks(null, true, false).OrderBy(t => t.ID).Select(t => t).ToList(); }
    //public List<Task> Completed { get => Edit ? Children.Where(n => n.Complete == 1d).Select(n => n.Task).ToList() : GetActionTasks(null, true, true).OrderBy(t => t.ID).Select(t => t).ToList(); }
    //public List<Task> ToDo { get => Edit ? Children.Where(n => n.Complete < 1d).Select(n => n.Task).ToList() : GetActionTasks(null, false, false).OrderBy(t => t.ID).Select(t => t).ToList(); }
    //public List<Task> Tasks
    //{
    //    get => Edit ? (All ? Children.Select(n => n.Task).ToList() : (Completed ? Children.Where(n => n.Complete == 1d).Select(n => n.Task).ToList() : Children.Where(n => n.Complete < 1d).Select(n => n.Task).ToList()))
    //                : (All ? GetActionTasks(null, true, false).OrderBy(t => t.ID).Select(t => t).ToList() : (Completed ? GetActionTasks(null, true, true).OrderBy(t => t.ID).Select(t => t).ToList() : GetActionTasks(null, false, false).OrderBy(t => t.ID).Select(t => t).ToList()));
    //}

    //private List<Task> GetActions(List<Task> actions, bool includeCompleted, bool onlyCompleted)
    //{
    //    if (null == actions)
    //        actions = new List<Task>();

    //    if (Children.Count == 0)
    //    {
    //        if (!actions.Contains(Task) && (includeCompleted ? true : Task.Complete < 1d) && (onlyCompleted ? Task.Complete == 1d : true))
    //            actions.Add(Task);
    //    }

    //    else Children.ForEach(n => n.GetActions(actions, includeCompleted, onlyCompleted));
    //    return actions;
    //}
    // Edit (default) (summary)    -- Completed/Todo/All   EDITABLE
    // Action         (leafs only) -- Completed/Todo/All   NOT EDITABLE

    //public List<Task> Actions                   //(leafs only) (not editable)
    //{
    //    get => Edit ? new List<Task>()
    //                : (All ? GetActions(null, true, false).OrderBy(t => t.ID).Select(t => t).ToList()
    //                : (Completed ? GetActions(null, true, true).OrderBy(t => t.ID).Select(t => t).ToList()
    //                : GetActions(null, false, false).OrderBy(t => t.ID).Select(t => t).ToList()));
    //}

    //private bool ContainsOrIs(Task potential)
    //{
    //    return ID == potential?.ID || Children.Where(c => c.ContainsOrIs(potential)).Count() > 0 || ((potential?.Children.Where(c => c.ContainsOrIs(this)).Count() ?? 0) > 0);
    //}
    internal class FileIO
    {
        public static bool DeleteFilesIn(string path, FilesFilter filter = null)
        {
            var returnValue = true;

            try
            {
                if (null == filter)
                    filter = FilesFilter.DefaultFilter;

                if (null != path && Directory.Exists(path))
                {
                    var currdir = Directory.GetCurrentDirectory();

                    Directory.SetCurrentDirectory(path);

                    var files = Directory
                        .EnumerateFiles(path)
                        .Where(f => new FilesFilter(f).Where(filter));

                    files.ToList().ForEach(f => File.Delete(f));

                    Directory.SetCurrentDirectory(currdir);         // Set the directory back to the previous directory
                }
            }
            catch (Exception)
            {
                returnValue = false;
            }

            return returnValue;
        }

        public static void ForEachFileIn(string path, Action<string> action)
        {
            if (Directory.Exists(path))
            {
                Directory.EnumerateFiles(path)
                .ToList()
                .ForEach(f => action?.Invoke(f));
            }
        }

        public static void ForEachSubdirectoryIn(string path, Action<string> action)
        {
            if (Directory.Exists(path))
            {
                Directory.EnumerateDirectories(path)
                .ToList()
                .ForEach(f => action?.Invoke(f));
            }
        }

        public static bool FileExists(string filepath)
        {
            return File.Exists(filepath);
        }

        public static string FilenameFrom(string filePath)
        {
            return File.Exists(filePath ?? "{nofile}")
                ? Path.GetFileName(filePath)
                : string.Empty;
        }

        public static string GetFullPathFrom(string relativePath)
        {
            var dir = AssemblyDirectory;
            Directory.SetCurrentDirectory(dir);

            while (!Directory.Exists(relativePath) && !File.Exists(Path.Combine(dir, relativePath)) && dir.Length > 3)
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

            var returnValue = (dir.Length <= 3) ? string.Empty : Path.Combine(dir, relativePath);       // Check for "C:/"
            Directory.SetCurrentDirectory(AssemblyDirectory);

            return returnValue;
        }

        private static string AssemblyDirectory
        {
            get
            {
                string codeBase = Assembly.GetEntryAssembly().CodeBase;
                UriBuilder uri = new UriBuilder(codeBase);
                string path = Uri.UnescapeDataString(uri.Path);
                return Path.GetDirectoryName(path);
            }
        }
    }

    internal class FilesFilter
    {
        public static FilesFilter DefaultFilter = new FilesFilter().Where("*.*");

        public FilesFilter(Predicate<DateTime> created, params string[] patterns) : this(patterns)
        {
            _created = created;
        }

        public FilesFilter(params string[] patterns) : this()
        {
            if (null != patterns)
                _namePatterns.AddRange(patterns);
        }

        public FilesFilter(string filename) : this()
        {
            _filename = (filename ?? string.Empty);
        }

        public FilesFilter()
        {
            _namePatterns = new List<string>();
            _notNamePatterns = new List<string>();
        }

        public FilesFilter(FilesFilter orig) : this()
        {
            if (string.Empty != (orig._filename ?? string.Empty))
                _filename = orig._filename;

            _namePatterns.AddRange(orig._namePatterns);
            _notNamePatterns.AddRange(orig._notNamePatterns);

            if (null != orig._created)
                _created = orig._created;

            if (null != orig._notcreated)
                _notcreated = orig._notcreated;
        }

        public static implicit operator bool(FilesFilter ff) => ff.EvalAnd();

        public static implicit operator FilesFilter(string[] input) => new FilesFilter(input);
        public static implicit operator FilesFilter(string input) => new FilesFilter(new string[] { input });

        public FilesFilter Where(FilesFilter initial)
        {
            var newone = new FilesFilter(this);

            if (string.Empty != (initial._filename ?? string.Empty))
                newone._filename = initial._filename;

            newone._namePatterns.AddRange(initial._namePatterns);
            newone._notNamePatterns.AddRange(initial._notNamePatterns);

            if (null != initial._created)
                newone._created = initial._created;

            if (null != initial._notcreated)
                newone._notcreated = initial._notcreated;

            return newone;
        }

        public FilesFilter Where(Predicate<DateTime> created)
        {
            return new FilesFilter(this) { _created = created };
        }

        public FilesFilter Where(params string[] patterns)
        {
            var newone = new FilesFilter(this);

            if (null != patterns)
                newone._namePatterns.AddRange(patterns);

            return newone;
        }

        public FilesFilter Not(FilesFilter initial)
        {
            var newone = new FilesFilter(this);

            if (string.Empty != (initial._filename ?? string.Empty))
                newone._filename = initial._filename;

            newone._notNamePatterns.AddRange(initial._namePatterns);

            if (null != initial._created)
                newone._notcreated = initial._created;

            return newone;
        }

        public FilesFilter Not(Predicate<DateTime> created)
        {
            return new FilesFilter(this) { _notcreated = created };
        }

        public FilesFilter Not(params string[] patterns)
        {
            var newone = new FilesFilter(this);

            if (null != patterns)
                newone._notNamePatterns.AddRange(patterns);

            return newone;
        }

        public bool EvalAnd()
        {
            if (string.Empty == (_filename ?? string.Empty))
                return false;

            var fi = (File.Exists(_filename)) ? new FileInfo(_filename) : null;

            if (null != _created)
            {
                if (null != fi)
                    if (!_created(fi.LastWriteTime))
                        return false;
            }

            if (null != _namePatterns)
            {
                if (null != fi)
                {
                    var tl = _namePatterns
                        .SelectMany(filter => Directory.EnumerateFiles(fi.DirectoryName, filter))
                        .Select(f => f.ToLower())
                        .ToList();

                    if (tl.Contains(_filename.ToLower()))
                        return EvalNot();

                    else return false;
                }
            }

            return EvalNot();
        }

        public bool EvalOr()
        {
            if (string.Empty == (_filename ?? string.Empty))
                return EvalNot();

            var fi = (File.Exists(_filename)) ? new FileInfo(_filename) : null;

            if (null != _created)
            {
                if (null != fi)
                    if (_created(fi.LastWriteTime))
                        return EvalNot();
            }

            if (null != _namePatterns)
            {
                if (null != fi)
                {
                    var tl = _namePatterns
                    .SelectMany(filter => Directory.EnumerateFiles(fi.DirectoryName, filter))
                    .Select(f => f.ToLower())
                    .ToList();

                    if (tl.Contains(_filename.ToLower()))
                        return EvalNot();
                }
            }

            return false;
        }

        private bool EvalNot()
        {
            if (string.Empty == (_filename ?? string.Empty))
                return true;

            var fi = (File.Exists(_filename)) ? new FileInfo(_filename) : null;
            if (null != _notcreated)
            {
                if (null != fi)
                    if (_notcreated(fi.LastWriteTime))
                        return false;
            }

            if (null != _notNamePatterns)
            {
                if (null != fi)
                {
                    var tl = _notNamePatterns
                    .SelectMany(filter => Directory.EnumerateFiles(fi.DirectoryName, filter))
                    .Select(f => f.ToLower())
                    .ToList();

                    return !tl.Contains(_filename.ToLower());
                }
            }

            return true;
        }

        private Predicate<DateTime> _created, _notcreated;
        private List<string> _namePatterns, _notNamePatterns;
        private string _filename;

        public override string ToString()
        {
            var sb = new StringBuilder();

            if (!string.IsNullOrWhiteSpace(_filename)) sb.AppendFormat("Filename == {0}", _filename ?? "{null}");
            if (sb.Length > 0) sb.AppendFormat("; ");
            if (_namePatterns != null && _namePatterns.Count > 0) { sb.AppendFormat("Patterns == {0}", string.Join(", ", _namePatterns ?? new List<string>()) ?? "{null}"); }
            if ((sb.Length > 0) && (!sb.ToString().EndsWith("; "))) sb.AppendFormat("; ");
            if (_notNamePatterns != null && _notNamePatterns.Count > 0) sb.AppendFormat("!Patterns == {0}", string.Join(", ", _notNamePatterns ?? new List<string>()) ?? "{null}");

            return "{ " + sb.ToString() + " }";
        }
    }
}
