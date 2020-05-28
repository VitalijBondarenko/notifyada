------------------------------------------------------------------------------
--                                                                          --
-- Copyright (c) 2014-2015 Vitalij Bondarenko <vibondare@gmail.com>         --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- The MIT License (MIT)                                                    --
--                                                                          --
-- Permission is hereby granted, free of charge, to any person obtaining a  --
-- copy of this software and associated documentation files (the            --
-- "Software"), to deal in the Software without restriction, including      --
-- without limitation the rights to use, copy, modify, merge, publish,      --
-- distribute, sublicense, and/or sell copies of the Software, and to       --
-- permit persons to whom the Software is furnished to do so, subject to    --
-- the following conditions:                                                --
--                                                                          --
-- The above copyright notice and this permission notice shall be included  --
-- in all copies or substantial portions of the Software.                   --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  --
-- OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               --
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   --
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY     --
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,     --
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE        --
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                   --
------------------------------------------------------------------------------

with Ada.Text_IO;         use Ada.Text_IO;

with Glib;                use Glib;
with Glib.Glist;          use Glib.Glist;
with Gtk.Enums;           use Gtk.Enums;
use Gtk.Enums.String_List;
with Gtk.Main;

with Notify;              use Notify;
with Notify.Notification; use Notify.Notification;

with GPS_Utils;           use GPS_Utils;

procedure Test is
   Notification : Notify_Notification;
   R            : Boolean;
   Name         : String_Ptr;
   Vendor       : String_Ptr;
   Version      : String_Ptr;
   Spec_Version : String_Ptr;
   Server_Caps  : String_List.Glist;

begin
   Restore_GPS_Startup_Values;
   Gtk.Main.Init;

   --  Init libnotyfy.
   R := Notify.Init ("Notify_Ada");

   --  Create  simple notification.
   G_New
     (Notification,
      "Test libnotify interface.",
      "Test Ada binding to the libnotify.",
      "");
   Set_Timeout (Notification, NOTIFY_EXPIRES_DEFAULT);
   R := Show (Notification);
   Unref (Notification);

   --  Create notification with icon.
   G_New
     (Notification,
      "Test libnotify interface.",
      "Test Ada binding to the libnotify.",
      "media-removable");
   R := Show (Notification);
   Unref (Notification);

   --  Create notification with customized body text.
   G_New
     (Notification,
      "Test libnotify interface.",
      "Some <b>bold</b>, <u>underlined</u>, <i>italic</i>, " &
        "<a href='http://www.google.com'>linked on Google</a> text",
      "");
   R := Show (Notification);
   Unref (Notification);

   --  Get and print information about server.
   R := Get_Server_Info (Name, Vendor, Version, Spec_Version);
   Put_Line ("Server information :");
   Put_Line ("Name : " & Name.all);
   Put_Line ("Vendor : " & Vendor.all);
   Put_Line ("Version : " & Version.all);
   Put_Line ("Spec version : " & Spec_Version.all);
   New_Line;

   --  Get and print information about server capabilities.
   Server_Caps := Get_Server_Caps;
   Put_Line ("Server capabilities :");
   while Server_Caps /= String_List.Null_List loop
      Put_Line (String_List.Get_Data (Server_Caps));
      Server_Caps := String_List.Next (Server_Caps);
   end loop;

   Free_String_List (Server_Caps);
end Test;
