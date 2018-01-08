{*****************************************************************}
{                 JSON RPC Web Service Library                    }
{                                                                 }
{ by Jose Benedito - josebenedito@gmail.com                       }
{ www.jbsolucoes.net                                              }
{*****************************************************************}

unit jwsconsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  JSONRPC_VERSION         : string = '2.0';
  JSON_HEADER_CONTENT_TYPE: string = 'Content-type: application/json; charset=UTF-8';

  ERR_INTERNAL_ERROR      = -32000;
  ERR_REQUEST_ERROR       = -32100;

  ERR_UNKNOW_FUNCTION     = -32601;
  ERR_REQUEST_CONTENT     = -32600;
  ERR_INVALID_CONTENT     = -32602;

  ERR_INVALID_QUERY       = -32300;

implementation

end.

