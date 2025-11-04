sap.ui.define([
    "sap/ui/core/UIComponent",
    "sap/ui/Device",
    "com/example/chat/model/models"
], function(UIComponent, Device, models) {
    "use strict";

    return UIComponent.extend("com.example.chat.Component", {
        metadata: {
            manifest: "json"
        },

        init: function() {
            UIComponent.prototype.init.apply(this, arguments);
            
            // Создаем и устанавливаем модель с данными
            var oMessageModel = models.createMessageModel();
            this.setModel(oMessageModel, "messages");
            
            // Убеждаемся, что модель установлена
            console.log("Модель установлена в компоненте:", this.getModel("messages") ? "да" : "нет");
            if (this.getModel("messages")) {
                var oData = this.getModel("messages").getData();
                console.log("Данные модели:", oData);
            }
        }
    });
});

