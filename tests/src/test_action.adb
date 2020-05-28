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

with Ada.Unchecked_Conversion;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

with Gtk.Main;
with Gtk.Enums;                use Gtk.Enums;
use Gtk.Enums.String_List;
with Glib;                     use Glib;
--  with Glib.Object;          use Glib.Object;
with Glib.Values;              use Glib.Values;

with Notify;                   use Notify;
with Notify.Notification;      use Notify.Notification;
with Test_Action_Callbacks;    use Test_Action_Callbacks;

with GPS_Utils;                use GPS_Utils;

procedure Test_Action is
   Notification : Notify_Notification;
   R            : Boolean;
   User_Data    : String_Ptr :=
     new UTF8_String'("String passed to the action callback");

   package My_Action is new Add_Action_User_Data (String_Ptr);

begin
   Restore_GPS_Startup_Values;
   Gtk.Main.Init;

   --  Init libnotify.
   R := Notify.Init ("Notify_Ada");

   --  Create new notification.
   G_New
     (Notification => Notification,
      Summary      => "Test libnotify interface.",
      Body_Text    => "Test adding action to a notification.",
      Icon_Name    => "");
   Set_Timeout (Notification, NOTIFY_EXPIRES_DEFAULT);

   --  Add action.
   Add_Action
     (Notification => Notification,
      Action       => "default",
      Label        => "Press Me",
      Callback     => Action_Callback'Access);

   --  Add action with user data.
   My_Action.Add_Action
     (Notification => Notification,
      Action       => "user_data_action",
      Label        => "Print Message",
      Callback     => Action_Callback_User_Data'Access,
      User_Data    => User_Data);

   --  Sets the category of the Notification.
   Set_Category (Notification, "presence.online");

   --  Connect signal "closed" handler.
   On_Closed (Notification, On_Closed_Callback'Access);

   --  Show Notification on the screen.
   R := Show (Notification);

   Gtk.Main.Main;
end Test_Action;
