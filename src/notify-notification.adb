------------------------------------------------------------------------------
--                                                                          --
-- Copyright (c) 2013-2015 Vitalij Bondarenko <vibondare@gmail.com>         --
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

with System;               use System;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Unchecked_Conversion;

with Glib.Values;
with Gtk.Arguments;        use Gtk.Arguments;
with Gtkada.Bindings;      use Gtkada.Bindings;

package body Notify.Notification is

   function Callback_To_Address is new Ada.Unchecked_Conversion
     (Notify_Action_Callback_Void, System.Address);

   function Callback_To_Address is new Ada.Unchecked_Conversion
     (G_Destroy_Notify_Address, System.Address);

   ----------------------------------
   -- Notify_Notification_Get_Type --
   ----------------------------------

   function Notify_Notification_Get_Type return Glib.GType is
      function Internal return GType;
      pragma Import (C, Internal, "notify_notification_get_type");

   begin
      return Internal;
   end Notify_Notification_Get_Type;

   -----------
   -- G_New --
   -----------

   procedure G_New
     (Notification : out Notify_Notification;
      Summary      : UTF8_String;
      Body_Text    : UTF8_String := "";
      Icon_Name    : UTF8_String := "")
   is
   begin
      Notification := new Notify_Notification_Record;
      Initialize (Notification, Summary, Body_Text, Icon_Name);
   end G_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Notification : access Notify_Notification_Record'Class;
      Summary      : UTF8_String;
      Body_Text    : UTF8_String;
      Icon_Name    : UTF8_String)
   is
      function Internal
        (Summary   : UTF8_String;
         Body_Text : UTF8_String;
         Icon_Name : UTF8_String)
         return System.Address;
      pragma Import (C, Internal, "notify_notification_new");

   begin
      Set_Object
        (Notification,
         Internal
           (Summary & ASCII.NUL,
            Body_Text & ASCII.NUL,
            Icon_Name & ASCII.NUL));
   end Initialize;

   ------------
   -- Update --
   ------------

   function Update
     (Notification : access Notify_Notification_Record;
      Summary      : UTF8_String;
      Body_Text    : UTF8_String := "";
      Icon_Name    : UTF8_String := "") return Boolean
   is
      function Internal
        (Notification : System.Address;
         Summary      : UTF8_String;
         Body_Text    : UTF8_String;
         Icon_Name    : UTF8_String)
         return Gboolean;
      pragma Import (C, Internal, "notify_notification_update");

   begin
      return 0 /= Internal
        (Get_Object (Notification),
         Summary & ASCII.NUL,
         Body_Text & ASCII.NUL,
         Icon_Name & ASCII.NUL);
   end Update;

   ----------
   -- Show --
   ----------

   function Show
     (Notification : access Notify_Notification_Record;
      Error        : access GError := null) return Boolean
   is
      function Internal
        (Notification : System.Address;
         Error        : access GError)
         return Gboolean;
      pragma Import (C, Internal, "notify_notification_show");

   begin
      return 0 /= Internal (Get_Object (Notification), Error);
   end Show;

   -----------------
   -- Set_Timeout --
   -----------------

   procedure Set_Timeout
     (Notification : access Notify_Notification_Record;
      Timeout      : Integer)
   is
      procedure Internal
        (Notification : System.Address;
         Timeout      : Integer);
      pragma Import (C, Internal, "notify_notification_set_timeout");

   begin
      Internal (Get_Object (Notification), Timeout);
   end Set_Timeout;

   ------------------
   -- Set_Category --
   ------------------

   procedure Set_Category
     (Notification : access Notify_Notification_Record;
      Category     : String)
   is
      procedure Internal
        (Notification : System.Address;
         Category     : String);
      pragma Import (C, Internal, "notify_notification_set_category");

   begin
      Internal (Get_Object (Notification), Category & ASCII.NUL);
   end Set_Category;

   -----------------
   -- Set_Urgency --
   -----------------

   procedure Set_Urgency
     (Notification : access Notify_Notification_Record;
      Urgency      : Notify_Urgency)
   is
      procedure Internal
        (Notification : System.Address;
         Urgency      : Notify_Urgency);
      pragma Import (C, Internal, "notify_notification_set_urgency");

   begin
      Internal (Get_Object (Notification), Urgency);
   end Set_Urgency;

   ---------------------------
   -- Set_Image_From_Pixbuf --
   ---------------------------

   procedure Set_Image_From_Pixbuf
     (Notification : access Notify_Notification_Record;
      Pixbuf       : Gdk_Pixbuf)
   is
      procedure Internal
        (Notification : System.Address;
         Pixbuf       : System.Address);
      pragma Import (C, Internal, "notify_notification_set_image_from_pixbuf");

   begin
      Internal (Get_Object (Notification), Get_Object (Pixbuf));
   end Set_Image_From_Pixbuf;

   --------------
   -- Set_Hint --
   --------------

   procedure Set_Hint
     (Notification : access Notify_Notification_Record;
      Key          : String;
      Value        : GVariant)
   is
      procedure Internal
        (Notification : System.Address;
         Key          : String;
         Value        : System.Address);
      pragma Import (C, Internal, "notify_notification_set_hint");

   begin
      Internal
        (Get_Object (Notification), Key & ASCII.NUL, Get_Object (Value));
   end Set_Hint;

   -----------------
   -- Clear_Hints --
   -----------------

   procedure Clear_Hints (Notification : access Notify_Notification_Record) is
      procedure Internal (Notification : System.Address);
      pragma Import (C, Internal, "notify_notification_clear_hints");

   begin
      Internal (Get_Object (Notification));
   end Clear_Hints;

   ----------------
   -- Add_Action --
   ----------------

   function To_Notify_Action_Callback_Void is new Ada.Unchecked_Conversion
     (System.Address, Notify_Action_Callback_Void);

   function To_Address is new Ada.Unchecked_Conversion
     (Notify_Action_Callback_Void, System.Address);

   procedure C_Action_Cb
     (Notification : System.Address;
      Action       : chars_ptr;
      User_Data    : System.Address);
   pragma Convention (C, C_Action_Cb);

   procedure C_Action_Cb
     (Notification : System.Address;
      Action       : chars_ptr;
      User_Data    : System.Address)
   is
      Proc              : constant Notify_Action_Callback_Void :=
        To_Notify_Action_Callback_Void (User_Data);
      Stub_Notification : Notify_Notification_Record;

   begin
      Proc
        (Notify_Notification (Get_User_Data (Notification, Stub_Notification)),
         Value (Action));
   end C_Action_Cb;

   procedure C_Notify_Notification_Add_Action
     (Notification : System.Address;
      Action       : chars_ptr;
      Label        : chars_ptr;
      Callback     : System.Address;
      User_Data    : System.Address;
      Free_Func    : System.Address);
   pragma Import
     (C, C_Notify_Notification_Add_Action, "notify_notification_add_action");

   procedure Add_Action
     (Notification : not null access Notify_Notification_Record;
      Action       : UTF8_String;
      Label        : UTF8_String;
      Callback     : Notify_Action_Callback_Void)
   is
   begin
      C_Notify_Notification_Add_Action
        (Get_Object (Notification),
         New_String (Action),
         New_String (Label),
         C_Action_Cb'Address,
         To_Address (Callback),
         System.Null_Address);
   end Add_Action;

   -------------------
   -- Clear_Actions --
   -------------------

   procedure Clear_Actions
     (Notification : access Notify_Notification_Record)
   is
      procedure Internal (Notification : System.Address);
      pragma Import (C, Internal, "notify_notification_clear_actions");

   begin
      Internal (Get_Object (Notification));
   end Clear_Actions;

   -----------
   -- Close --
   -----------

   function Close
     (Notification : access Notify_Notification_Record;
      Error        : access GError := null)
      return Boolean
   is
      function Internal
        (Notification : System.Address;
         Error        : access GError)
         return Gboolean;
      pragma Import (C, Internal, "notify_notification_close");

   begin
      return 0 /= Internal (Get_Object (Notification), Error);
   end Close;

   -----------------------
   -- Get_Closed_Reason --
   -----------------------

   function Get_Closed_Reason
     (Notification : access Notify_Notification_Record)
      return Integer
   is
      function Internal (Notification : System.Address) return Integer;
      pragma Import (C, Internal, "notify_notification_get_closed_reason");

   begin
      return Internal (Get_Object (Notification));
   end Get_Closed_Reason;

   ----------------------------------
   -- package Add_Action_User_Data --
   ----------------------------------

   package body Add_Action_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Notify_Action_Callback_Void is new Ada.Unchecked_Conversion
        (System.Address, Notify_Action_Callback_Void);

      function To_Address is new Ada.Unchecked_Conversion
        (Notify_Action_Callback_Void, System.Address);

      procedure C_Action_Cb
        (Notification : System.Address;
         Action       : chars_ptr;
         User_Data    : System.Address);
      pragma Convention (C, C_Action_Cb);

      procedure C_Action_Cb
        (Notification : System.Address;
         Action       : chars_ptr;
         User_Data    : System.Address)
      is
         D                 : constant Users.Internal_Data_Access :=
           Users.Convert (User_Data);
         Stub_Notification : Notify_Notification_Record;

      begin
         To_Notify_Action_Callback_Void (D.Func)
           (Notify_Notification
              (Get_User_Data (Notification, Stub_Notification)),
            Value (Action),
            D.Data.all);
      end C_Action_Cb;

      procedure Add_Action
        (Notification : not null access Notify_Notification_Record'Class;
         Action       : UTF8_String;
         Label        : UTF8_String;
         Callback     : Notify_Action_Callback_Void;
         User_Data    : User_Data_Type)
      is
      begin
         C_Notify_Notification_Add_Action
           (Get_Object (Notification),
            New_String (Action),
            New_String (Label),
            C_Action_Cb'Address,
            Users.Build (To_Address (Callback), User_Data),
            Users.Free_Data'Address);
      end Add_Action;

   end Add_Action_User_Data;

   ----------------------
   --  Signal handling --
   ----------------------

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Notify_Notification_Void, System.Address);

   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Notify_Notification_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
     (Object  : access Notify_Notification_Record'Class;
      C_Name  : Glib.Signal_Name;
      Handler : Cb_Notify_Notification_Void;
      After   : Boolean);

   procedure Connect_Slot
     (Object  : access Notify_Notification_Record'Class;
      C_Name  : Glib.Signal_Name;
      Handler : Cb_GObject_Void;
      After   : Boolean;
      Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Void
     (Closure         : GClosure;
      Return_Value    : Glib.Values.GValue;
      N_Params        : Glib.Guint;
      Params          : Glib.Values.C_GValues;
      Invocation_Hint : System.Address;
      User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Notify_Notification_Void
     (Closure         : GClosure;
      Return_Value    : Glib.Values.GValue;
      N_Params        : Glib.Guint;
      Params          : Glib.Values.C_GValues;
      Invocation_Hint : System.Address;
      User_Data       : System.Address);
   pragma Convention (C, Marsh_Notify_Notification_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Object  : access Notify_Notification_Record'Class;
      C_Name  : Glib.Signal_Name;
      Handler : Cb_Notify_Notification_Void;
      After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object     => Object,
         C_Name     => C_Name,
         Marshaller => Marsh_Notify_Notification_Void'Access,
         Handler    => Cb_To_Address (Handler),
         After      => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
     (Object  : access Notify_Notification_Record'Class;
      C_Name  : Glib.Signal_Name;
      Handler : Cb_GObject_Void;
      After   : Boolean;
      Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Void'Access,
         Handler     => Cb_To_Address (Handler),
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------------
   -- Marsh_GObject_Void --
   ------------------------

   procedure Marsh_GObject_Void
     (Closure         : GClosure;
      Return_Value    : Glib.Values.GValue;
      N_Params        : Glib.Guint;
      Params          : Glib.Values.C_GValues;
      Invocation_Hint : System.Address;
      User_Data       : System.Address)
   is
      pragma Unreferenced
        (Return_Value, N_Params, Params, Invocation_Hint, User_Data);

      H   : constant Cb_GObject_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject :=
        Glib.Object.Convert (Get_Data (Closure));

   begin
      H (Obj);
   exception when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   ------------------------------------
   -- Marsh_Notify_Notification_Void --
   ------------------------------------

   procedure Marsh_Notify_Notification_Void
     (Closure         : GClosure;
      Return_Value    : Glib.Values.GValue;
      N_Params        : Glib.Guint;
      Params          : Glib.Values.C_GValues;
      Invocation_Hint : System.Address;
      User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);

      H   : constant Cb_Notify_Notification_Void :=
        Address_To_Cb (Get_Callback (Closure));
      Obj : constant Notify_Notification :=
        Notify_Notification (Unchecked_To_Object (Params, 0));

   begin
      H (Obj);
   exception when E : others => Process_Exception (E);
   end Marsh_Notify_Notification_Void;

   ---------------
   -- On_Closed --
   ---------------

   procedure On_Closed
     (Self  : not null access Notify_Notification_Record;
      Call  : Cb_Notify_Notification_Void;
      After : Boolean := False)
   is
   begin
      Connect (Self, Signal_Closed & ASCII.NUL, Call, After);
   end On_Closed;

   procedure On_Closed
     (Self  : not null access Notify_Notification_Record;
      Call  : Cb_GObject_Void;
      Slot  : not null access Glib.Object.GObject_Record'Class;
      After : Boolean := False)
   is
   begin
      Connect_Slot (Self, Signal_Closed & ASCII.NUL, Call, After, Slot);
   end On_Closed;

end Notify.Notification;
