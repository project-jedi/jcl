
const
   SOL_IPX = 256;
   IPX_TYPE = 1;
   IPX_NODE_LEN = 6;
   IPX_MTU = 576;

type
   Psockaddr_ipx = ^sockaddr_ipx;
   sockaddr_ipx = record
        sipx_family : sa_family_t;
        sipx_port : u_int16_t;
        sipx_network : u_int32_t;
        sipx_node : array[0..(IPX_NODE_LEN)-1] of byte;
        sipx_type : u_int8_t;
        sipx_zero : byte;
     end;


const
   IPX_DLTITF = 0;
   IPX_CRTITF = 1;

type

   Pipx_route_definition = ^ipx_route_definition;
   ipx_route_definition = record
        ipx_network : dword;
        ipx_router_network : dword;
        ipx_router_node : array[0..(IPX_NODE_LEN)-1] of byte;
     end;

   Pipx_interface_definition = ^ipx_interface_definition;
   ipx_interface_definition = record
        ipx_network : dword;
        ipx_device : array[0..15] of byte;
        ipx_dlink_type : byte;
        ipx_special : byte;
        ipx_node : array[0..(IPX_NODE_LEN)-1] of byte;
     end;

const
   IPX_FRAME_NONE = 0;
   IPX_FRAME_SNAP = 1;
   IPX_FRAME_8022 = 2;
   IPX_FRAME_ETHERII = 3;
   IPX_FRAME_8023 = 4;
   IPX_FRAME_TR_8022 = 5;
   IPX_SPECIAL_NONE = 0;
   IPX_PRIMARY = 1;
   IPX_INTERNAL = 2;
type

   Pipx_config_data = ^ipx_config_data;
   ipx_config_data = record
        ipxcfg_auto_select_primary : byte;
        ipxcfg_auto_create_interfaces : byte;
     end;
   Pipx_route_def = ^ipx_route_def;
   ipx_route_def = record
        ipx_network : dword;
        ipx_router_network : dword;
        ipx_router_node : array[0..(IPX_NODE_LEN)-1] of byte;
        ipx_device : array[0..15] of byte;
        ipx_flags : word;
     end;


const
   IPX_ROUTE_NO_ROUTER = 0;
   IPX_RT_SNAP = 8;
   IPX_RT_8022 = 4;
   IPX_RT_BLUEBOOK = 2;
   IPX_RT_ROUTED = 1;
   SIOCAIPXITFCRT = SIOCPROTOPRIVATE;
   SIOCAIPXPRISLT = SIOCPROTOPRIVATE + 1;
   SIOCIPXCFGDATA = SIOCPROTOPRIVATE + 2;
   SIOCIPXNCPCONN = SIOCPROTOPRIVATE + 3;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
