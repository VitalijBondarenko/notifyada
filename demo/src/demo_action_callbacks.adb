------------------------------------------------------------------------------
--                                                                          --
-- Copyright (c) 2014-2023 Vitalii Bondarenko <vibondare@gmail.com>         --
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

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

with Glib;                     use Glib;
with Gtk;                      use Gtk;
with Gtk.Main;

package body Demo_Action_Callbacks is

   ---------------------
   -- Action_Callback --
   ---------------------

   procedure Action_Callback
     (Notification : Notify_Notification;
      Action       : UTF8_String)
   is
   begin
      Put_Line ("Action : " & Action);
      Gtk.Main.Main_Quit;
      Put_Line ("Activation Token : " & Notification.Get_Activation_Token);
      Put_Line ("Action button pressed." & ASCII.LF & "Goodbye!");
   end Action_Callback;

   -------------------------------
   -- Action_Callback_User_Data --
   -------------------------------

   procedure Action_Callback_User_Data
     (Notification : Notify_Notification;
      Action       : UTF8_String;
      User_Data    : String_Ptr)
   is
   begin
      Put_Line ("Action : " & Action);
      Put_Line ("User Data : " & User_Data.all);
      Gtk.Main.Main_Quit;
      Put_Line ("Action button pressed." & ASCII.LF & "Goodbye!");
   end Action_Callback_User_Data;

   ------------------------
   -- On_Closed_Callback --
   ------------------------

   procedure On_Closed_Callback
     (Notification : access Notify_Notification_Record'Class)
   is
   begin
      Gtk.Main.Main_Quit;
      Put_Line ("Notification closed." & ASCII.LF & "Goodbye!");
   end On_Closed_Callback;

end Demo_Action_Callbacks;
