ListCell* listReverse (ListCell* list) {
  ListCell* result = list;
  while (list) {
    ListCell* newCell = kernelNewListCell (list->data, result);
    result = newCell;
    list = list->next;
  }
  return result;
}

ListCell* listRest (ListCell* list) {
  return list->next;
}
