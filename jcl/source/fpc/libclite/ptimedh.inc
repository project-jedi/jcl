
const
   TSPVERSION = 1;
   ANYADDR = Nil;

type
   Ptsp = ^tsp;
   tsp = record
        tsp_type : u_char;
        tsp_vers : u_char;
        tsp_seq : u_short;
        tsp_u : record
            case longint of
               0 : ( tspu_time : timeval );
               1 : ( tspu_hopcnt : char );
            end;
        tsp_name : array[0..(MAXHOSTNAMELEN)-1] of char;
     end;


const
   TSP_ANY = 0;
   TSP_ADJTIME = 1;
   TSP_ACK = 2;
   TSP_MASTERREQ = 3;
   TSP_MASTERACK = 4;
   TSP_SETTIME = 5;
   TSP_MASTERUP = 6;
   TSP_SLAVEUP = 7;
   TSP_ELECTION = 8;
   TSP_ACCEPT = 9;
   TSP_REFUSE = 10;
   TSP_CONFLICT = 11;
   TSP_RESOLVE = 12;
   TSP_QUIT = 13;
   TSP_DATE = 14;
   TSP_DATEREQ = 15;
   TSP_DATEACK = 16;
   TSP_TRACEON = 17;
   TSP_TRACEOFF = 18;
   TSP_MSITE = 19;
   TSP_MSITEREQ = 20;
   TSP_TEST = 21;
   TSP_SETDATE = 22;
   TSP_SETDATEREQ = 23;
   TSP_LOOP = 24;
   TSPTYPENUMBER = 25;

   tsptype : array [0..TSPTYPENUMBER-1] of pchar =
  (
   'ANY', 'ADJTIME', 'ACK', 'MASTERREQ', 'MASTERACK', 'SETTIME', 'MASTERUP',
   'SLAVEUP', 'ELECTION', 'ACCEPT', 'REFUSE', 'CONFLICT', 'RESOLVE', 'QUIT',
  'DATE', 'DATEREQ', 'DATEACK', 'TRACEON', 'TRACEOFF', 'MSITE', 'MSITEREQ',
  'TEST', 'SETDATE', 'SETDATEREQ', 'LOOP' );

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
