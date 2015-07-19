table status : {Id : int, Nam : string}
  PRIMARY KEY Id
  CONSTRAINT Nam UNIQUE Nam

table t : {
  Id : int, 
  Nam : string, 
  Ready : bool, 
  ForeignStatus : int }
  PRIMARY KEY Id
  CONSTRAINT ForeignStatus 
    FOREIGN KEY ForeignStatus 
    REFERENCES status(Id)

open Crud.Make(struct
  val tab = t
  
  val title = "Are you Ready? Select a Foreign Status."

  val cols = {
    Nam = Crud.string "Name",

    Ready = {
      Nam = "Ready?",

      Show = (fn b => 
       if b then
         <xml>Ready!</xml>
       else
         <xml>Not ready</xml>),

      Widget = (fn [nm :: Name] => 
        <xml>
          <select{nm}>
            <option>Ready</option>
            <option>Not ready</option>
          </select>
        </xml>),

      WidgetPopulated = (fn [nm :: Name] b => 
      <xml>
        <select{nm}>
          <option selected={b}>Ready</option>
          <option selected={not b}>Not ready</option>
        </select>
      </xml>),

      Parse = (fn s =>
        case s of
            "Ready" => True
          | "Not ready" => False
          | _ => error <xml>Invalid ready/not ready</xml>),

      Inject = _
    },

    ForeignStatus = {
      Nam = "Foreign Status",
  
      Show = (fn fstat =>
        let
          val statusNamTxn = queryL1 (SELECT * FROM status WHERE Id = {[fstat]})
          val statusNam = nth (List.MapX (fn r => r.Nam) statusNamTxn) 1
        in
          case statusNam of
              Some sn => <xml>{sn}</xml>
            | _ => error <xml>Error: Show: Foreign Status<xml>
        end),

      Widget = (fn [nm :: Name] => 
        let
          val statusOptionsTxn = 
            queryL1 (SELECT Id, Nam FROM status ORDER BY Nam) 
          val statusOptions = 
            List.MapX 
              (fn r => <xml><option>{r.Nam}</option></xml>)
              statusOptionsTxn
        in
          <xml>
            <select{nm}>
              {statusOptions}
            </select>
          </xml>),

      WidgetPopulated = (fn [nm :: Name] fstatId => 
        let
          val statusOptionsPopTxn = 
            queryL1 (SELECT status.Id, status.Nam FROM status ORDER BY status.Nam) 
          val statusOptionsPop = 
            List.MapX
              (fn r => <xml><option selected={r.Id=fstatId}>{r.Nam}</option></xml>)
              statusOptionsPopTxn
        in
          <xml>
            <select{nm}>
              {statusOptionsPop}
            </select>
          </xml>
         end),

      Parse = (fn fstatNam =>
        let
          val fstatIdTxn = queryL1 (SELECT Id FROM status WHERE Nam = {[fstatNam]})
          val fstatId = 
            nth (List.MapX (fn r => f.fstatId) fStatIdTxn) 1
        in
          case fstatId of
              Some fsid => fsid
            | _ error <xml>Error: Parse: Foreign Status<xml>
        end),

      Inject = _
    }
  }
end)