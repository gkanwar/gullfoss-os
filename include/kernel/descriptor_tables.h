#ifndef DESCRIPTOR_TABLES_H
#define DESCRIPTOR_TABLES_H


struct DescTables;

class DescTablesManager {
 public:
  DescTablesManager();
  static DescTablesManager& get();
  void initialize(void* tables_page);
 private:
  DescTables* tables;
};

#endif
