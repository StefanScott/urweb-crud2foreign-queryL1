open List

table status : {Id : int, Nam : string}
  PRIMARY KEY Id
  CONSTRAINT Nam UNIQUE Nam

table t : {
  Id : int, 
  Nam : string, 
  Status : int }
  PRIMARY KEY Id
  CONSTRAINT Status 
    FOREIGN KEY Status 
    REFERENCES status(Id)

open Crud.Make(struct
  val tab = t
  
  val title = "Select a Status."

  val cols = {
    Nam = Crud.string "Name",

    Status = {
      Nam = "tatus",
  
      Show = (fn statusId =>
        let
          val statusList = queryL1 (SELECT * FROM status WHERE Id = {[statusId]})
          val statusNam = List.nth (List.mapX (fn r => r.Nam) statusList) 1
        in
          case statusNam of
              Some sn => <xml>{sn}</xml>
            | _ => error <xml>Error: Show: bad status.Id</xml>
        end),

      Widget = (fn [nm :: Name] => 
        let
          val statusOptionsList = 
            queryL1 (SELECT Id, Nam FROM status ORDER BY Nam) 
          val statusOptions = 
            List.mapX 
              (fn r => <xml><option>{r.Nam}</option></xml>)
              statusOptionsList
        in
          <xml>
            <select{nm}>
              {statusOptions}
            </select>
          </xml>
        end),

      WidgetPopulated = (fn [nm :: Name] statusId => 
        let
          val statusOptionsList = 
            queryL1 (SELECT status.Id, status.Nam FROM status ORDER BY status.Nam) 
          val statusOptionsPop = 
            List.mapX
              (fn r => <xml><option selected={r.Id=statusId}>{r.Nam}</option></xml>)
              statusOptionsList
        in
          <xml>
            <select{nm}>
              {statusOptionsPop}
            </select>
          </xml>
         end),

      Parse = (fn statusNam =>
        let
          val statusList = queryL1 (SELECT * FROM status WHERE Nam = {[statusNam]})
          val statusId = nth (List.mapX (fn r => r.Id) statusList) 1
        in
          case statusId of
              Some si => si
            | _ => error <xml>Error: Parse: bad status.Nam</xml>
        end),

      Inject = _
    }
  }
end)